-- The module where all the tracing actually happens

module RayTrace (renderScene, 
                 findNearestIntersection, 
                 findAnyIntersection, 
                 GlobalIlluminationFunc, 
                 PathTraceContext(PathTraceContext, samplesPerPixelRoot, maxPathBounces, samplesOverHemisphere, hemisphereGatherDistance)) where

import PolymorphicNum
import Vector
import {-# SOURCE #-} Light
import Primitive
import Colour
import Ray
import Material
import Matrix
import Misc
import Camera
import Distribution
import SceneGraph
import Control.Parallel.Strategies
import {-# SOURCE #-} PhotonMap (PhotonMap, irradiance)
import IrradianceCache
import Control.Monad.State
import RenderContext
import System.Random.Mersenne.Pure64
import Data.List
import RussianRoulette
import Control.DeepSeq

data PathTraceContext = PathTraceContext {
      samplesPerPixelRoot :: Int, -- sqrt of the total number of samples per pixel
      maxPathBounces :: Int,
      samplesOverHemisphere :: Int, -- Number of samples to trace over the hemisphere used to estimate irradiance at first bounce
      hemisphereGatherDistance :: Double
    }

-- Intersect a ray against a sphere tree
intersectSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double, Int) -> Maybe (Object, Double, Int)
intersectSphereTree (node:nodes) ray currentHit = intersectSphereTree (newNodeList ++ nodes) newRay thisResult
    where
      -- Intersect the ray with the bounding volume of this node
      (thisResult, newNodeList, newRay) = 
          case children node of
            -- If the node has no children, don't bother with it's bounding volume and just check the object (if it has one)
            [] -> case object node of
                    Nothing -> error "A node with no children should hold an object"
                    Just obj -> case primitiveClosestIntersect (primitive obj) ray obj of
                                  -- Didn't hit the object. Retain the current hit, and continue with remaining nodes on the list
                                  Nothing -> (currentHit, [], ray)
                                  -- We did hit this object. Update the intersection, and continue with remaining nodes on the list
                                  Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), [], shortenRay ray objHitDistance)
            -- We have children. In this case it makes sense to test our bounding volume
            nodeChildren -> case sphereIntersect (boundingRadius node) (boundingCentre node) ray of -- (make a sphere centred at the object's transform matrix with given radius)
                              -- If we do not find an intersection, we do not update the results and we offer no further nodes to be traversed, thus skipping this subtree
                              Nothing -> (currentHit, [], ray)
                              -- If we do find an intersection against the bounding volume, then we try again against the actual object (if present)
                              Just _ -> case object node of
                                          Nothing -> (currentHit, nodeChildren, ray) -- No object; just pass to the children
                                          Just obj -> case primitiveClosestIntersect (primitive obj) ray obj of
                                                        -- Didn't hit the object. Retain the current hit, but offer up the children of the node as we hit the bounding volume
                                                        Nothing -> (currentHit, nodeChildren, ray)
                                                        -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                        Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), nodeChildren, shortenRay ray objHitDistance)
intersectSphereTree [] _ currentHit = currentHit

-- Intersect with the list of infinite objects
intersectObjectList :: [Object] -> Ray -> Maybe (Object, Double, Int) -> Maybe (Object, Double, Int)
intersectObjectList (obj:objs) ray currentHit = intersectObjectList objs newRay thisResult
    where
      (thisResult, newRay) = case primitiveClosestIntersect (primitive obj) ray obj of
                               Nothing -> (currentHit, ray)
                               Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), shortenRay ray objHitDistance)
intersectObjectList [] _ currentHit = currentHit

-- Find the nearest intersection along a line
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, Int)
findNearestIntersection sceneGraph' ray = case intersectObjectList (infiniteObjects sceneGraph') ray Nothing of
                                            Just (obj, dist, objId) -> intersectSphereTree [root sceneGraph'] (shortenRay ray dist) (Just (obj, dist, objId))
                                            Nothing -> intersectSphereTree [root sceneGraph'] ray Nothing

-- Intersect a ray against a scene graph. Return first (ie, any) hit without finding the closest
findAnyIntersectionSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double)
findAnyIntersectionSphereTree (node:nodes) ray = case sphereIntersect (boundingRadius node) (boundingCentre node) ray of -- (make a sphere centred at the object's transform matrix with given radius)
                                                   -- If we do not find an intersection, traverse to the rest of the list
                                                   Nothing -> findAnyIntersectionSphereTree nodes ray
                                                   -- If we do find an intersection against the bounding volume, then we try again against the actual object
                                                   Just _ -> case object node of
                                                               Nothing -> findAnyIntersectionSphereTree (children node ++ nodes) ray -- No object here - just offer up the children
                                                               Just obj -> case primitiveAnyIntersect (primitive obj) ray obj of 
                                                                             -- Didn't hit the object. Offer up the children of the scene graph node to continue with (as we did actually hit the bounding volume)
                                                                             Nothing -> findAnyIntersectionSphereTree (children node ++ nodes) ray
                                                                             -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                                             Just (objHitDistance, _) -> Just (obj, objHitDistance)
findAnyIntersectionSphereTree [] _ = Nothing

-- Find any intersection against an object list
findAnyIntersectionObjectList :: [Object] -> Ray -> Maybe (Object, Double)
findAnyIntersectionObjectList (obj:objs) ray = case primitiveAnyIntersect (primitive obj) ray obj of
                                                 -- Didn't hit he object. Retain the current hit, but offer up the children of the node as we hit the bounding volume
                                                 Nothing -> findAnyIntersectionObjectList objs ray
                                                 -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                 Just (objHitDistance, _) -> Just (obj, objHitDistance)
findAnyIntersectionObjectList [] _ = Nothing

findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Double)
findAnyIntersection sceneGraph' ray = case findAnyIntersectionObjectList (infiniteObjects sceneGraph') ray of
                                        Nothing -> findAnyIntersectionSphereTree [root sceneGraph'] ray
                                        Just x -> Just x

-- Default background colour to return when we can't match anything
defaultColour :: Direction -> Colour
defaultColour _ = colBlue

-- Accumulate the contributions of the lights
lightSurface :: [Light] -> Colour -> RenderContext -> SurfaceLocation -> Material -> Vector -> Colour
lightSurface (x:xs) acc renderContext posTanSpace objMaterial viewDirection 
    = let result = acc <+> emissive <+> lightRadiance
          lightRadiance = applyLight (sceneGraph renderContext) posTanSpace objMaterial viewDirection x
          emissive = emission objMaterial
      in seq result (lightSurface xs result renderContext posTanSpace objMaterial viewDirection)
lightSurface [] acc _ _ _ _ = acc

-- Abstraction to permit different GI calculations
type GlobalIlluminationFunc = (SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache))

-- Photon map specific GI calculator
photonMapGlobalIllumination :: Maybe PhotonMap -> SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache)
photonMapGlobalIllumination (Just photonMap) surfaceLocation irrCache obj renderContext = 
    case renderMode renderContext of
      PhotonMapper -> if useIrradianceCache renderContext
                      then query irrCache surfaceLocation irradiance'
                      else (fst $ irradiance photonMap (photonMapContext renderContext) (material obj) surfaceLocation, irrCache)
      _ -> undefined -- Shouldn't hit this path...
    where
      irradiance' = irradiance photonMap (photonMapContext renderContext) (material obj)
photonMapGlobalIllumination _ _ irrCache _ _ = (colBlack, irrCache)

-- Null GI
nullGI :: SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache)
nullGI _ irrCache _ _ = (colBlack, irrCache)

-- Retrieve the appropriate GI function and calculate the GI at a point
calculateGI :: RenderContext -> Maybe PhotonMap -> GlobalIlluminationFunc
calculateGI renderContext photonMap = case renderMode renderContext of
                                        PhotonMapper -> photonMapGlobalIllumination photonMap
                                        _ -> nullGI

-- Perform a full trace of a ray
type RayTraceState = State (IrradianceCache, PureMT) Colour
traceRay :: RenderContext -> Maybe PhotonMap -> Ray -> Int -> Direction -> Double -> Double -> RayTraceState

-- Special case for lowest level of recursion (theoretically this should not get hit)
traceRay _ _ _ 0 _ _ _ = error "Should not hit this codepath"

-- Special case for penultimate level - we're not allowed to spawn rays here
traceRay renderContext photonMap ray 1 viewDir _ _ = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, hitId) -> do
          (irrCache, mt) <- get
          let intersectionPoint = pointAlongRay ray intersectionDistance
          let tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
          let (surfaceIrradiance, irrCache') = calculateGI renderContext photonMap (intersectionPoint, tanSpace) irrCache obj renderContext
          -- TODO - Need to plug irradiance values into shader model correctly
          let resultColour = lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) (material obj) viewDir
          put (irrCache', mt)
          return $! resultColour

-- General case
traceRay renderContext photonMap ray limit viewDir currentIOR accumulatedReflectivity = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, hitId) -> do
          -- Evaluate surface-location specific things such as shader results
          let intersectionPoint = pointAlongRay ray intersectionDistance
          let tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
          let normal = thr tanSpace
          let incoming = Vector.negate $ direction ray
          -- TODO - Need to plug irradiance values into shader model correctly

          -- Evaluate result from irradiance cache
          (irrCache, mt) <- get
          let (surfaceIrradiance, irrCache') = calculateGI renderContext photonMap (intersectionPoint, tanSpace) irrCache obj renderContext
          put (irrCache', mt)

          let surfaceShading = lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) (material obj) viewDir

          -- Reflection specific code
          let offsetToExterior = madd intersectionPoint normal surfaceEpsilon
          let reflectionDir = normalise $ reflect incoming normal
          let shine = reflectivity $ material obj
          let reflectRay = rayWithDirection offsetToExterior reflectionDir (reflectionRayLength renderContext)
          let (reflection, (irrCache'', mt')) = if shine > 0 && (accumulatedReflectivity * shine) > 0.03 
                                                then runState (traceRay renderContext photonMap reflectRay (limit - 1) viewDir currentIOR (accumulatedReflectivity * shine)) (irrCache', mt)
                                                else (colBlack, (irrCache', mt))
          put (irrCache'', mt')

          -- Refraction specific
          let eta = if enteringObject incoming normal
                    then currentIOR / indexOfRefraction (material obj) 
                    else indexOfRefraction (material obj) / currentIOR
          let refractionDir = normalise $ refract incoming normal eta
          let offsetToInterior = madd intersectionPoint refractionDir surfaceEpsilon
          let transmittance = transmit $ material obj
          let refractRay = rayWithDirection offsetToInterior refractionDir (refractionRayLength renderContext)
          let (refraction, (irrCache''', mt'')) = if transmittance > 0 
                                                  then runState (traceRay renderContext photonMap refractRay (limit - 1) viewDir (indexOfRefraction $ material obj) accumulatedReflectivity) (irrCache'', mt')
                                                  else (colBlack, (irrCache'', mt'))
          put (irrCache''', mt'')

          -- Final colour combine
          return $! (surfaceShading <+> reflection <*> shine <+> refraction <*> transmittance)
              where
                enteringObject incoming normal = incoming `dot3` normal > 0

-- Work out the irradiance over a hemisphere at a point in space
irradianceOverHemisphere :: RenderContext -> Int -> SurfaceLocation -> Direction -> Double -> PathTraceState
irradianceOverHemisphere renderContext numSamples (pos, tanSpace) viewDir gatherDistance =
    do
      gen <- get
      let (hemisphereDirs, gen') = generateDirectionsOnHemisphere numSamples 1 gen
      put gen'
      let samples = map (\x -> let ray = rayWithDirection pos (transformDir x tanSpace) gatherDistance
                               in case findNearestIntersection (sceneGraph renderContext) ray of
                                 Nothing -> colBlack
                                 Just (obj, dist, hitId) -> let hitPoint = pointAlongRay ray dist
                                                                hitTanSpace = primitiveTangentSpace (primitive obj) hitId hitPoint obj
                                                            in lightSurface (lights renderContext) colBlack renderContext (hitPoint, hitTanSpace) (material obj) viewDir
                        ) 
                    hemisphereDirs
      return $! averageColour samples -- Reduce samples down

-- Trace a list of distributed samples with tail recursion
rayTracePixelSample :: Maybe PhotonMap -> RenderContext -> Colour -> [Position] -> (Position, Direction) -> Double -> RayTraceState
rayTracePixelSample photonMap renderContext acc (x:xs) eyeViewDir sampleWeighting = 
    do
      let dofFocalDistance = depthOfFieldFocalDistance renderContext
      let jitteredRayPosition jitter = fst eyeViewDir <+> jitter
      let jitteredRayDirection jitter = normalise $ madd jitter (snd eyeViewDir) dofFocalDistance

      (irrCache, mt) <- get
      let (sampleColour, (irrCache', mt')) = runState (traceRay renderContext photonMap (rayWithDirection (jitteredRayPosition x) (jitteredRayDirection x) 100000.0) (maximumRayDepth renderContext) (snd eyeViewDir) 1 1) (irrCache, mt)
      put (irrCache', mt')

      let acc' = sampleColour <*> sampleWeighting <+> acc
      let (col, (irrCache'', mt'')) = runState (rayTracePixelSample photonMap renderContext acc' xs eyeViewDir sampleWeighting) (irrCache', mt')
      put (irrCache'', mt'')
      return $! col
rayTracePixelSample _ _ acc [] _ _ = return $! acc

-- This traces for a given pixel (x, y)
rayTracePixel :: Maybe PhotonMap -> RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> RayTraceState
rayTracePixel photonMap renderContext camera (width, height) (x, y) = 
    let eye = Camera.position camera
        rayDirection = makeRayDirection width height camera (fromIntegral x, fromIntegral (height - 1 - y))
    in do 
      (irrCache, mt) <- get
      let (distributedPositions, mt') = generatePointsOnSphere (numDistribSamples renderContext) (rayOriginDistribution renderContext) mt
      let (pixelColour, (irrCache', mt'')) = runState (rayTracePixelSample photonMap renderContext colBlack distributedPositions (eye, rayDirection) (1.0 / (fromIntegral . numDistribSamples $ renderContext))) (irrCache, mt')
      put (irrCache', mt'')
      return $! pixelColour

-- This function converts a pixel co-ordinate to a direction of the ray
makeRayDirection :: Int -> Int -> Camera -> (Double, Double) -> Vector
makeRayDirection renderWidth renderHeight camera (x, y) =
    let x' = (x / fromIntegral renderWidth) * 2.0 - 1.0
        y' = (y / fromIntegral renderHeight) * 2.0 - 1.0
        fov = 0.5 * fieldOfView camera
        fovX = tan (degreesToRadians fov)
        fovY = -tan (degreesToRadians fov)
        aspectRatio = fromIntegral renderWidth / fromIntegral renderHeight
        dirX = fovX * x'
        dirY = fovY * (-y') / aspectRatio
        rayDir = normalise (Vector dirX dirY 1 0)
    in normalise $ transformVector (worldToCamera camera) rayDir

type PathTraceState = State PureMT Colour

-- Trace a path, throough a scene
pathTrace :: RenderContext -> Ray -> Int -> Direction -> Double -> Colour -> PathTraceState
pathTrace renderContext ray depth viewDir currentIOR weight =
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! colBlack
        Just (obj, intersectionDistance, hitId) ->           
            -- Evaluate surface-location specific things such as shader results
            let intersectionPoint = pointAlongRay ray intersectionDistance
                shadingPoint = madd intersectionPoint normal surfaceEpsilon
                tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
                normal = thr tanSpace
                incoming = Vector.negate $ direction ray

                objMaterial = material obj

                -- Thunk for emitted light
                emittedLight = emission objMaterial
            
                -- Compute radiance at this point
                radiance = lightSurface (lights renderContext) colBlack renderContext (intersectionPoint, tanSpace) (material obj) viewDir

                -- Perhaps these should be precalculated?
                (diffuseP, specularP) = russianRouletteCoefficients objMaterial

                -- Don't just let things keep going, put a lid on it. Very diminishing returns after 10 bounces
                bounceLimit = 10
            in do
              -- TODO - Need to evaluate the shader model here!

              -- Randomly decide the fate at this intersection
              gen <- get
              let (p, gen') = randomDouble gen
              put gen'
              let interaction | depth >= bounceLimit = Absorb
                              | depth <= 5 || p < diffuseP = DiffuseReflect
                              | p < (diffuseP + specularP) = SpecularReflect
                              | otherwise = Absorb

              -- Thunk for reflected light 
              let (randomDir, gen'') = generateDirectionOnHemisphere gen' 1
              put gen''
              let reflectedDir 
                      | interaction == DiffuseReflect = transformDir randomDir tanSpace
                      | otherwise = incoming `reflect` normal
              let ray' = rayWithDirection (madd intersectionPoint normal surfaceEpsilon) reflectedDir (rayLength ray)
              let weight' = (diffuse . material) obj <*> weight <*> (normal `sdot3` reflectedDir)
              -- Handled the depth < maxBounces test in two places as it was causing an infinite recursion with deepseq
              -- For the first hit, run an extra in-depth gather for the irradiance
              -- For subsequent bounces, just do a single ray
              let (tracedPathColour, gen''') 
                      | depth == 0 = runState (irradianceOverHemisphere renderContext 256 (shadingPoint, tanSpace) viewDir 5000) gen''
                      | depth < bounceLimit && Colour.magnitude weight' > 0.001 = runState (pathTrace renderContext ray' (depth + 1) viewDir currentIOR weight') gen''
                      | otherwise = (colBlack, gen'')

--              let (tracedPathColour, gen''') = runState (irradianceOverHemisphere renderContext 256 (shadingPoint, tanSpace) viewDir 5000) gen''
              let reflectedLight = tracedPathColour <*> weight'
              put gen'''

--              return $!tracedPathColour

              -- Have to divide by probability to correctly account for that relative proportion of the domain
              return $! case interaction of DiffuseReflect -> (emittedLight <+> radiance <+> reflectedLight) </> diffuseP
                                            SpecularReflect -> (emittedLight <+> radiance <+> reflectedLight) </> (specularP - diffuseP)
                                            Absorb -> (emittedLight <+> radiance) </> (1 - (specularP `Prelude.max` diffuseP))

-- Path-trace a sub-sample
pathTracePixelSample :: RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> (Double, Double) -> (Double, Double) -> PathTraceState
pathTracePixelSample renderContext camera xy (width, height) jitterUV stratUV = pathTrace renderContext ray 0 rayDirection 1 colWhite
    where
      jitteredX = (fromIntegral . fst) xy + fst stratUV + fst jitterUV
      jitteredY = (fromIntegral . snd) xy + snd stratUV + snd jitterUV
      rayDirection = makeRayDirection width height camera (jitteredX, jitteredY)
      ray = rayWithDirection (Camera.position camera) rayDirection (farClip camera)

-- Path-trace a pixel. Do stratified sub-sampling
pathTracePixel :: RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> PathTraceState
pathTracePixel renderContext camera renderTargetSize pixelCoords = 
    do
      -- Work out the jittered UV offsets
      gen <- get
      let (offsetUVs, gen') = runState (generateRandomUVs numPathTraceSamples) gen
      let offsetUVs' = map (\(u, v) -> (u * du, v * dv)) offsetUVs
      put gen'

      -- Mung it all together
      let (pixelSamples, gen'') = zipWithState (pathTracePixelSample renderContext camera pixelCoords renderTargetSize) offsetUVs' stratifiedCentres gen'
      put gen''
      return $! pixelSamples `deepseq` foldl' (\x y -> x <*> weight <+> y) colBlack pixelSamples
    where
      -- Total number of samples to take
      numPathTraceSamplesRoot = 6 :: Int
      numPathTraceSamples = numPathTraceSamplesRoot * numPathTraceSamplesRoot
      weight = (1.0 :: Double) / fromIntegral numPathTraceSamples

      -- Work out a set of stratified centres to jitter from
      du = (1.0 :: Double) / fromIntegral numPathTraceSamplesRoot
      dv = du
      stratifiedCentres = [(fromIntegral x * du, fromIntegral y * dv) | y <- [0..(numPathTraceSamplesRoot - 1)], x <- [0..(numPathTraceSamplesRoot - 1)]]

-- Generate a list of colours which contains a raytraced image. In parallel
rayTraceImage :: Maybe PhotonMap -> RenderContext -> Camera -> Int -> Int -> [Colour]
rayTraceImage photonMap renderContext camera renderWidth renderHeight = mapWithStateDiscard pixelCoords (irrCache, pureMT 12345) (rayTracePixel photonMap renderContext camera (renderWidth, renderHeight))
                                                                        `using` parListChunk 256 rdeepseq
    where 
      pixelCoords = [(x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
      irrCache = initialiseCache (sceneGraph renderContext)

pathTraceImage :: RenderContext -> Camera -> Int -> Int -> [Colour]
pathTraceImage renderContext camera renderWidth renderHeight = zipWith'
                                                               (\x y -> evalState (pathTracePixel renderContext camera (renderWidth, renderHeight) x) (pureMT y))
                                                               [(x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
                                                               [1..] 
                                                               `using` parListChunk 16 rdeepseq

-- Perform a full render of the frame
renderScene :: Maybe PhotonMap -> RenderContext -> Camera -> Int -> Int -> [Colour]
renderScene photonMap renderContext camera width height = case renderMode renderContext of
                                                            PathTracer -> pathTraceImage renderContext camera width height
                                                            _ -> rayTraceImage photonMap renderContext camera width height
