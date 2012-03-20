-- The module where all the tracing actually happens

module RayTrace (renderScene, 
                 findNearestIntersection, 
                 findAnyIntersection, 
                 irradianceOverHemisphere,
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
import Camera
import Distribution
import SceneGraph
import Control.Parallel.Strategies
import {-# SOURCE #-} PhotonMap (PhotonMap, irradiance)
import IrradianceCache
import Control.Monad.State
import RenderContext
import System.Random
import RussianRoulette
import Misc
import {-# SOURCE #-} ShadowCache

data PathTraceContext = PathTraceContext {
      samplesPerPixelRoot :: Int, -- sqrt of the total number of samples per pixel
      maxPathBounces :: Int,
      samplesOverHemisphere :: Int, -- Number of samples to trace over the hemisphere used to estimate irradiance at first bounce
      hemisphereGatherDistance :: Double
    }

-- Intersect a ray against a sphere tree
intersectSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double, TangentSpace) -> Maybe (Object, Double, TangentSpace)
intersectSphereTree (node:nodes) ray currentHit = intersectSphereTree (newNodeList ++ nodes) newRay thisResult
    where
      -- Intersect the ray with the bounding volume of this node
      (thisResult, newNodeList, newRay) = 
          case children node of
            -- If the node has no children, don't bother with it's bounding volume and just check the object (if it has one)
            [] -> case object node of
                    Nothing -> (currentHit, [], ray) -- error "A node with no children should hold an object"
                    Just obj -> case primitiveClosestIntersect (primitive obj) ray obj of
                                  -- Didn't hit the object. Retain the current hit, and continue with remaining nodes on the list
                                  Nothing -> (currentHit, [], ray)
                                  -- We did hit this object. Update the intersection, and continue with remaining nodes on the list
                                  Just (objHitDistance, objHitTanSpace) -> (Just (obj, objHitDistance, objHitTanSpace), [], shortenRay ray objHitDistance)
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
                                                        Just (objHitDistance, objHitTanSpace) -> (Just (obj, objHitDistance, objHitTanSpace), nodeChildren, shortenRay ray objHitDistance)
intersectSphereTree [] _ currentHit = currentHit

-- Intersect with the list of infinite objects
intersectObjectList :: [Object] -> Ray -> Maybe (Object, Double, TangentSpace) -> Maybe (Object, Double, TangentSpace)
intersectObjectList (obj:objs) ray currentHit = intersectObjectList objs newRay thisResult
    where
      (thisResult, newRay) = case primitiveClosestIntersect (primitive obj) ray obj of
                               Nothing -> (currentHit, ray)
                               Just (objHitDistance, objHitTanSpace) -> (Just (obj, objHitDistance, objHitTanSpace), shortenRay ray objHitDistance)
intersectObjectList [] _ currentHit = currentHit

-- Find the nearest intersection along a line
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, TangentSpace)
findNearestIntersection sceneGraph' ray = case intersectObjectList (infiniteObjects sceneGraph') ray Nothing of
                                            Just (obj, dist, tanSpace) -> intersectSphereTree [root sceneGraph'] (shortenRay ray dist) (Just (obj, dist, tanSpace))
                                            Nothing -> intersectSphereTree [root sceneGraph'] ray Nothing

-- Intersect a ray against a scene graph. Return first (ie, any) hit without finding the closest
findAnyIntersectionSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double, TangentSpace)
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
                                                                             Just (d, tanSpace) -> Just (obj, d, tanSpace)
findAnyIntersectionSphereTree [] _ = Nothing

-- Find any intersection against an object list
findAnyIntersectionObjectList :: [Object] -> Ray -> Maybe (Object, Double, TangentSpace)
findAnyIntersectionObjectList (obj:objs) ray = case primitiveAnyIntersect (primitive obj) ray obj of
                                                 -- Didn't hit he object. Retain the current hit, but offer up the children of the node as we hit the bounding volume
                                                 Nothing -> findAnyIntersectionObjectList objs ray
                                                 -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                 Just (d, tanSpace) -> Just (obj, d, tanSpace)
findAnyIntersectionObjectList [] _ = Nothing

findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, TangentSpace)
findAnyIntersection sceneGraph' ray = case findAnyIntersectionObjectList (infiniteObjects sceneGraph') ray of
                                        Nothing -> findAnyIntersectionSphereTree [root sceneGraph'] ray
                                        Just x -> Just x

-- Default background colour to return when we can't match anything
defaultColour :: Direction -> Colour
defaultColour _ = colBlue

-- Accumulate the contributions of the lights
type LightSurfaceState = State ShadowCache Colour
lightSurface :: [Light] -> Colour -> RenderContext -> SurfaceLocation -> Material -> Vector -> LightSurfaceState
lightSurface (x:xs) acc renderContext posTanSpace objMaterial viewDirection =
  do
    let lightRadiance = applyLight (sceneGraph renderContext) posTanSpace objMaterial viewDirection x
    result <- lightSurface xs (acc <+> lightRadiance) renderContext posTanSpace objMaterial viewDirection
    return $! result
lightSurface [] acc _ _ _ _ = return $! acc

-- Abstraction to permit different GI calculations
type GlobalIlluminationFunc = (SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache))

-- Photon map specific GI calculator
photonMapGlobalIllumination :: Maybe PhotonMap -> SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache)
photonMapGlobalIllumination (Just photonMap) surfaceLocation irrCache obj renderContext = 
    case renderMode renderContext of
      PhotonMapper -> if useIrradianceCache renderContext
                      then query irrCache surfaceLocation irradiance'
                      else (fst $ irradiance photonMap (photonMapContext renderContext) (material obj) surfaceLocation, irrCache)
      _ -> error "Called photonMapGlobalIllumination when photon mapping was not selected."
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
type RayTraceState g = State (IrradianceCache, g, ShadowCache) Colour
traceRay :: (RandomGen g) => RenderContext -> Maybe PhotonMap -> Ray -> Int -> Camera -> Double -> Double -> RayTraceState g

-- Special case for lowest level of recursion (theoretically this should not get hit)
traceRay _ _ _ 0 _ _ _ = error "Should not hit this codepath"

-- Special case for penultimate level - we're not allowed to spawn rays here
traceRay renderContext photonMap ray 1 camera _ _ = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, tanSpace) -> do
          (irrCache, mt, shadowCache) <- get
          let intersectionPoint = pointAlongRay ray intersectionDistance
          let (surfaceIrradiance, irrCache') = calculateGI renderContext photonMap (intersectionPoint, tanSpace) irrCache obj renderContext
          -- Deliberately chosen not to shade the fetched irradiance value by the material
          -- This may seem immediately strange, but the reason is that the value generated should already have been appropriately shaded
          -- by the diffuse contributions of the materials and shaders. So we should just sum it, like we do with the local lighting values
              
          -- We only accumulate the ambient colour for the first hit. Otherwise we would erroneously accumulate it many times over
          let objMaterial = (material obj) { ambient = colBlack } 
          let viewDir = normalise (intersectionPoint <-> Camera.position camera)
          let (surfaceLighting, shadowCache') = runState (lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) objMaterial viewDir) shadowCache
          let resultColour = emission objMaterial <+> surfaceLighting
          put (irrCache', mt, shadowCache')
          return $! resultColour

-- General case
traceRay renderContext photonMap ray limit camera currentIOR accumulatedReflectivity = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, tanSpace) -> do
          -- Evaluate surface-location specific things such as shader results
          let intersectionPoint = pointAlongRay ray intersectionDistance
          let normal = thr tanSpace
          let incoming = Vector.negate $ direction ray

          -- Evaluate result from irradiance cache
          (irrCache, mt, shadowCache) <- get
          let (surfaceIrradiance, irrCache') = calculateGI renderContext photonMap (intersectionPoint, tanSpace) irrCache obj renderContext
          put (irrCache', mt, shadowCache)
          -- Deliberately chosen not to shade the fetched irradiance value by the material
          -- This may seem immediately strange, but the reason is that the value generated should already have been appropriately shaded
          -- by the diffuse contributions of the materials and shaders. So we should just sum it, like we do with the local lighting values

          -- We only accumulate the ambient colour for the first hit. Otherwise we would erroneously accumulate it many times over
          let objMaterial 
                | limit == maximumRayDepth renderContext = material obj
                | otherwise = (material obj) { ambient = colBlack } 

          let viewDir = normalise (intersectionPoint <-> Camera.position camera)
          let (surfaceLighting, shadowCache') = runState(lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) objMaterial viewDir) shadowCache
          let surfaceShading = emission objMaterial <+> surfaceLighting
          put (irrCache', mt, shadowCache')

          -- Reflection specific code
          let offsetToExterior = madd intersectionPoint normal surfaceEpsilon
          let reflectionDir = normalise $ reflect incoming normal
          let shine = reflectivity $ material obj
          let reflectRay = rayWithDirection offsetToExterior reflectionDir (reflectionRayLength renderContext)
          st <- get
          let (reflection, st') = if shine > 0 && (accumulatedReflectivity * shine) > 0.03 
                                  then runState (traceRay renderContext photonMap reflectRay (limit - 1) camera currentIOR (accumulatedReflectivity * shine)) st
                                  else (colBlack, st)
          put st'
          
          -- Refraction specific
          let eta = if enteringObject incoming normal
                    then currentIOR / indexOfRefraction (material obj) 
                    else indexOfRefraction (material obj) / currentIOR
          let refractionDir = normalise $ refract incoming normal eta
          let offsetToInterior = madd intersectionPoint refractionDir surfaceEpsilon
          let transmittance = transmit $ material obj
          let refractRay = rayWithDirection offsetToInterior refractionDir (refractionRayLength renderContext)
          let (refraction, st'') = if transmittance > 0 
                                   then runState (traceRay renderContext photonMap refractRay (limit - 1) camera (indexOfRefraction $ material obj) accumulatedReflectivity) st'
                                   else (colBlack, st')
          put st''

          -- Final colour combine
          return $! (surfaceShading <+> reflection <*> shine <+> refraction <*> transmittance)
              where
                enteringObject incoming normal = incoming `dot3` normal > 0

-- Work out the irradiance over a hemisphere at a point in space
irradianceOverHemisphere :: (RandomGen g) => RenderContext -> Int -> SurfaceLocation -> Direction -> Double -> PathTraceState g
irradianceOverHemisphere renderContext numSamples (pos, tanSpace) viewDir gatherDistance =
    do
      (gen, shadowCache) <- get
      let (hemisphereDirs, gen') = generateStratifiedDirectionsOnHemisphere numSamples 1 gen
      put (gen', shadowCache)
      let samples = map (\x -> let gatherDir = transformDir x tanSpace
                                   ray = rayWithDirection pos gatherDir gatherDistance
                               in case findNearestIntersection (sceneGraph renderContext) ray of
                                 Nothing -> colBlack
                                 Just (obj, dist, hitTanSpace) -> let hitPoint = pointAlongRay ray dist
                                                                  in evalState (lightSurface (lights renderContext) colBlack renderContext (hitPoint, hitTanSpace) (material obj) viewDir) initShadowCache
                        ) 
                    hemisphereDirs
      return $! averageColour samples -- Reduce samples down

-- Trace a list of distributed samples with tail recursion
rayTracePixelSample :: (RandomGen g) => Maybe PhotonMap -> RenderContext -> Colour -> [Position] -> (Position, Direction) -> Double -> Camera -> RayTraceState g
rayTracePixelSample photonMap renderContext acc (x:xs) eyeViewDir sampleWeighting camera = 
    do
      let dofFocalDistance = depthOfFieldFocalDistance renderContext
      let jitteredRayPosition jitter = fst eyeViewDir <+> jitter
      let jitteredRayDirection jitter = normalise $ madd jitter (snd eyeViewDir) dofFocalDistance

      st <- get
      let (sampleColour, st') = runState (traceRay renderContext photonMap (rayWithDirection (jitteredRayPosition x) (jitteredRayDirection x) 100000.0) (maximumRayDepth renderContext) camera 1 1) st
      put st'

      let acc' = sampleColour <*> sampleWeighting <+> acc
      let (col, st'') = runState (rayTracePixelSample photonMap renderContext acc' xs eyeViewDir sampleWeighting camera) st'
      put st''
      return $! col
rayTracePixelSample _ _ acc [] _ _ _ = return $! acc

-- This traces for a given pixel (x, y)
rayTracePixel :: (RandomGen g) => Maybe PhotonMap -> RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> RayTraceState g
rayTracePixel photonMap renderContext camera (width, height) (x, y) = 
    let eye = Camera.position camera
        rayDirection = makeRayDirection width height camera (fromIntegral x, fromIntegral (height - 1 - y))
    in do 
      (irrCache, mt, shadowCache) <- get
      let (distributedPositions, mt') = generatePointsOnSphere (numDistribSamples renderContext) (rayOriginDistribution renderContext) mt
      let (pixelColour, (irrCache', mt'', shadowCache')) = runState (rayTracePixelSample photonMap renderContext colBlack distributedPositions (eye, rayDirection) (1.0 / (fromIntegral . numDistribSamples $ renderContext)) camera) (irrCache, mt', shadowCache)
      put (irrCache', mt'', shadowCache')
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

type PathTraceState g = State (g, ShadowCache) Colour

-- Trace a path, throough a scene
pathTrace :: (RandomGen g) => RenderContext -> Ray -> Int -> Double -> Colour -> Camera -> State (g, ShadowCache) Colour
pathTrace _ _ 10 _ _ _ = return $! colBlack
pathTrace renderContext ray depth currentIOR weight camera =
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! colBlack
        Just (hitObj, hitDistance, tanSpace) -> 
          do 
            -- Use russian roulette to decide fate at each interaction
            (rndGen, shadowCache) <- get            
            let (p, rndGen') = runState randDouble rndGen
            put (rndGen', shadowCache)
            
            let (interaction, probability) | depth >= bounceLimit = (Absorb, 0)
                                           | p < diffuseP = (DiffuseReflect, 1.0 / diffuseP)
                                           | p < specularP = (SpecularReflect, 1.0 / specularP)
                                           | depth < minBounces && diffuseP > 0 = (DiffuseReflect, 1.0 / diffuseP)
                                           | depth < minBounces && specularP > 0 = (DiffuseReflect, 1.0 / specularP)
                                           | otherwise = (Absorb, 0)

            -- Get reflected light
            let (randomDir, rndGen'') = runState (generateUnstratifiedDirectionOnHemisphere 1) rndGen'
            put (rndGen'', shadowCache)

            let reflectedDir 
                  | interaction == SpecularReflect = incoming `reflect` normal
                  | otherwise = transformDir randomDir tanSpace
            let ray' = rayWithDirection shadingPoint reflectedDir 10000
            let weight' = weight <*> diffuse objMaterial <*> (normal `sdot3` reflectedDir)
            let (radiance, shadowCache') = runState (lightSurface (lights renderContext) colBlack renderContext (shadingPoint, tanSpace) objMaterial viewDir) shadowCache
            put (rndGen'', shadowCache')

            reflectedLight <- pathTrace renderContext ray' (depth + 1) currentIOR weight' camera
                                 
            -- Combine to give a result
            return $! emittedLight <+> radiance <*> weight <+> reflectedLight <*> probability

            -- Evaluate surface-location specific things such as shader results
          where hitPoint = pointAlongRay ray hitDistance
                shadingPoint = madd hitPoint normal surfaceEpsilon
                normal = tsNormal tanSpace
                incoming = (Vector.negate . direction) ray

                -- We only accumulate the ambient colour for the first hit. Otherwise we would erroneously accumulate it many times over
                objMaterial 
                  | depth == 0 = material hitObj
                  | otherwise = (material hitObj) { ambient = colBlack } 

                -- Emitted light
                emittedLight = emission objMaterial
            
                -- Compute radiance at this point
                viewDir = normalise (shadingPoint <-> Camera.position camera)
                --radiance = lightSurface (lights renderContext) colBlack renderContext (shadingPoint, tanSpace) objMaterial viewDir

                -- Perhaps these should be precalculated?
                (diffuseP, specularP) = russianRouletteCoefficients objMaterial
                                        
                -- Don't just let things keep going, put a lid on it. Very diminishing returns after 10 bounces
                bounceLimit = 10
                minBounces = 3

-- Path-trace a sub-sample
pathTracePixelSample :: (RandomGen g) => RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> PathTraceState g
pathTracePixelSample renderContext camera (x, y) (width, height) (du, dv) (jitterU, jitterV) (stratU, stratV) = pathTrace renderContext ray 0 1 colWhite camera
    where
      jitteredX = fromIntegral x + (stratU + jitterU) * du
      jitteredY = fromIntegral y + (stratV + jitterV) * dv
      rayDirection = makeRayDirection width height camera (jitteredX, jitteredY)
      ray = rayWithDirection (Camera.position camera) rayDirection (farClip camera)

-- Path-trace a pixel. Do stratified sub-sampling
pathTracePixel :: (RandomGen g) => RenderContext -> Camera -> (Int, Int) -> (Int, Int) -> PathTraceState g
pathTracePixel renderContext camera renderTargetSize pixelCoords = 
    do
      -- Work out the jittered UV offsets
      (gen, shadowCache) <- get
      let (offsetUVs, gen') = runState (generateRandomUVs numPathTraceSamples) gen
      put (gen', shadowCache)

      -- Mung it all together
      let (pixelSamples, finalState) = zipWithState (pathTracePixelSample renderContext camera pixelCoords renderTargetSize (du, dv)) offsetUVs stratifiedCentres (gen', shadowCache)
      put finalState
      return $! averageColour pixelSamples
    where
      -- Total number of samples to take
      numPathTraceSamplesRoot = 32 :: Int
      numPathTraceSamples = numPathTraceSamplesRoot * numPathTraceSamplesRoot

      -- Work out a set of stratified centres to jitter from
      du = (1.0 :: Double) / fromIntegral numPathTraceSamplesRoot
      dv = du
      stratifiedCentres = [(fromIntegral x, fromIntegral y) | y <- [0..(numPathTraceSamplesRoot - 1)], x <- [0..(numPathTraceSamplesRoot - 1)]]

-- Generate a list of colours which contains a raytraced image. In parallel
rayTraceImage :: Maybe PhotonMap -> RenderContext -> Camera -> Int -> Int -> [Colour]
rayTraceImage photonMap renderContext camera renderWidth renderHeight = mapWithStateDiscard pixelCoords (irrCache, mkStdGen 12345, initShadowCache) (rayTracePixel photonMap renderContext camera (renderWidth, renderHeight))
                                                                        `using` parListChunk 256 rdeepseq
    where 
      pixelCoords = [(x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
      irrCache = initialiseCache (sceneGraph renderContext)

pathTraceImage :: RenderContext -> Camera -> Int -> Int -> [Colour]
pathTraceImage renderContext camera renderWidth renderHeight = zipWith'
                                                               (\x y -> evalState (pathTracePixel renderContext camera (renderWidth, renderHeight) x) (mkStdGen y, initShadowCache))
                                                               [(x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
                                                               [1..] 
                                                               `using` parListChunk 16 rdeepseq

-- Perform a full render of the frame
renderScene :: Maybe PhotonMap -> RenderContext -> Camera -> Int -> Int -> [Colour]
renderScene photonMap renderContext camera width height = case renderMode renderContext of
                                                            PathTracer -> pathTraceImage renderContext camera width height
                                                            _ -> rayTraceImage photonMap renderContext camera width height
