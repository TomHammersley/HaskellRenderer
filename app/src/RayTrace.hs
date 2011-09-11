-- The module where all the tracing actually happens
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module RayTrace (rayTraceImage, findNearestIntersection, findAnyIntersection, GlobalIlluminationFunc) where

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

-- Intersect a ray against a sphere tree
intersectSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double, Int) -> Maybe (Object, Double, Int)
intersectSphereTree !(node:nodes) !ray !currentHit = seq result (intersectSphereTree (newNodeList ++ nodes) newRay thisResult)
    where
      -- Intersect the ray with the bounding volume of this node
      !result = case children node of
                 -- If the node has no children, don't bother with it's bounding volume and just check the object (if it has one)
                 [] -> case object node of
                         Nothing -> error "A node with no children should hold an object"
                         Just obj -> case primitiveClosestIntersect (primitive obj) ray obj of
                                       -- Didn't hit the object. Retain the current hit, and continue with remaining nodes on the list
                                       Nothing -> (currentHit, [], ray)
                                        -- We did hit this object. Update the intersection, and continue with remaining nodes on the list
                                       Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), [], shortenRay ray objHitDistance)
                                        -- We have children. In this case it makes sense to test our bounding volume
                 !nodeChildren -> case sphereIntersect (boundingRadius node) (boundingCentre node) ray of -- (make a sphere centred at the object's transform matrix with given radius)
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
      (!thisResult, !newNodeList, !newRay) = result
intersectSphereTree [] _ !currentHit = currentHit

-- Intersect with the list of infinite objects
intersectObjectList :: [Object] -> Ray -> Maybe (Object, Double, Int) -> Maybe (Object, Double, Int)
intersectObjectList !(obj:objs) !ray !currentHit = intersectObjectList objs newRay thisResult
    where
      (!thisResult, !newRay) = case primitiveClosestIntersect (primitive obj) ray obj of
                                 Nothing -> (currentHit, ray)
                                 Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), shortenRay ray objHitDistance)
intersectObjectList [] _ !currentHit = currentHit

-- Find the nearest intersection along a line
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, Int)
findNearestIntersection sceneGraph' !ray = case intersectObjectList (infiniteObjects sceneGraph') ray Nothing of
                                             Just (obj, dist, objId) -> intersectSphereTree [root sceneGraph'] (shortenRay ray dist) (Just (obj, dist, objId))
                                             Nothing -> intersectSphereTree [root sceneGraph'] ray Nothing

-- Intersect a ray against a scene graph. Return first (ie, any) hit without finding the closest
findAnyIntersectionSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Double)
findAnyIntersectionSphereTree (node:nodes) !ray = let sphereIntersectionResult = sphereIntersect (boundingRadius node) (boundingCentre node) ray
                                                  in case sphereIntersectionResult of -- (make a sphere centred at the object's transform matrix with given radius)
                                                       -- If we do not find an intersection, traverse to the rest of the list
                                                       Nothing -> findAnyIntersectionSphereTree nodes ray
                                                       -- If we do find an intersection against the bounding volume, then we try again against the actual object
                                                       Just _ -> case object node of
                                                                   Nothing -> findAnyIntersectionSphereTree (nodes ++ children node) ray -- No object here - just offer up the children
                                                                   Just obj -> case primitiveAnyIntersect (primitive obj) ray obj of 
                                                                                 -- Didn't hit the object. Offer up the children of the scene graph node to continue with (as we did actually hit the bounding volume)
                                                                                 Nothing -> findAnyIntersectionSphereTree (nodes ++ children node) ray
                                                                                  -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                                                 Just (objHitDistance, _) -> Just (obj, objHitDistance)
findAnyIntersectionSphereTree [] _ = Nothing

-- Find any intersection against an object list
findAnyIntersectionObjectList :: [Object] -> Ray -> Maybe (Object, Double)
findAnyIntersectionObjectList (obj:objs) !ray = case primitiveAnyIntersect (primitive obj) ray obj of
                                                  -- Didn't hit he object. Retain the current hit, but offer up the children of the node as we hit the bounding volume
                                                  Nothing -> findAnyIntersectionObjectList objs ray
                                                  -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                  Just (objHitDistance, _) -> Just (obj, objHitDistance)
findAnyIntersectionObjectList [] _ = Nothing

findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Double)
findAnyIntersection sceneGraph' !ray = case findAnyIntersectionObjectList (infiniteObjects sceneGraph') ray of
                                        Nothing -> findAnyIntersectionSphereTree [root sceneGraph'] ray
                                        Just x -> Just x

-- Default background colour to return when we can't match anything
defaultColour :: Direction -> Colour
defaultColour _ = colBlue

-- Accumulate the contributions of the lights
lightSurface :: [Light] -> Colour -> RenderContext -> SurfaceLocation -> Material -> Vector -> Colour
lightSurface (x:xs) !acc renderContext !posTanSpace !objMaterial !viewDirection 
    = let result = acc + applyLight (sceneGraph renderContext) posTanSpace objMaterial viewDirection x
      in seq result (lightSurface xs result renderContext posTanSpace objMaterial viewDirection)
lightSurface [] !acc _ _ _ _ = acc

irrCacheSampleRadius :: Double
irrCacheSampleRadius = 5

-- Abstraction to permit different GI calculations
type GlobalIlluminationFunc = (SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache))

-- Photon map specific GI calculator
photonMapGlobalIllumination :: Maybe PhotonMap -> SurfaceLocation -> IrradianceCache -> Object -> RenderContext -> (Colour, IrradianceCache)
photonMapGlobalIllumination (Just photonMap) !surfaceLocation irrCache obj renderContext = case renderMode renderContext of
                                                                                             PhotonMapper -> query irrCache surfaceLocation irradiance'
                                                                                             _ -> (colBlack, irrCache) -- Shouldn't really hit this path...
    where
      irradiance' x = (irradiance photonMap (photonMapContext renderContext) (material obj) x, irrCacheSampleRadius)
photonMapGlobalIllumination _ _ irrCache _ _ = (colBlack, irrCache)

-- Perform a full trace of a ray
type RayTraceState = State IrradianceCache Colour
traceRay :: RenderContext -> Maybe PhotonMap -> Ray -> Int -> Direction -> Double -> Double -> RayTraceState

-- Special case for lowest level of recursion (theoretically this should not get hit)
traceRay _ _ _ 0 _ _ _ = error "Should not hit this codepath"

-- Special case for penultimate level - we're not allowed to spawn rays here
traceRay renderContext photonMap !ray 1 !viewDir _ _ = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, hitId) -> do
          irrCache <- get
          let !intersectionPoint = pointAlongRay ray intersectionDistance
          let !tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
          let !(!surfaceIrradiance, !newIrrCache) = (photonMapGlobalIllumination photonMap) (intersectionPoint, tanSpace) irrCache obj renderContext
          -- TODO - Need to plug irradiance values into surface shading more correctly
          let resultColour = lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) (material obj) viewDir
          put newIrrCache
          return $! resultColour

-- General case
traceRay renderContext photonMap !ray !limit !viewDir !currentIOR !accumulatedReflectivity = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> return $! defaultColour (direction ray)
        Just (obj, intersectionDistance, hitId) -> do
          -- Evaluate surface-location specific things such as shader results
          let !intersectionPoint = pointAlongRay ray intersectionDistance
          let !tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
          let !normal = thr tanSpace
          let !incoming = Vector.negate $ direction ray
          -- TODO - Need to plug irradiance values into surface shading more correctly

          -- Evaluate result from irradiance cache
          irrCache <- get
          let !(!surfaceIrradiance, !irrCache') = (photonMapGlobalIllumination photonMap) (intersectionPoint, tanSpace) irrCache obj renderContext
          put irrCache'

          let !surfaceShading = lightSurface (lights renderContext) surfaceIrradiance renderContext (intersectionPoint, tanSpace) (material obj) viewDir

          -- Reflection specific code
          let offsetToExterior = madd intersectionPoint normal surfaceEpsilon
          let reflectionDir = normalise $ reflect incoming normal
          let !shine = reflectivity $ material obj
          let reflectRay = rayWithDirection offsetToExterior reflectionDir (reflectionRayLength renderContext)
          let (reflection, irrCache'') = if shine > 0 && (accumulatedReflectivity * shine) > 0.03 
                                         then runState (traceRay renderContext photonMap reflectRay (limit - 1) viewDir currentIOR (accumulatedReflectivity * shine)) irrCache'
                                         else (colBlack, irrCache')
          put irrCache''

          -- Refraction specific
          let !eta = if enteringObject incoming normal
                     then currentIOR / indexOfRefraction (material obj) 
                     else indexOfRefraction (material obj) / currentIOR
          let refractionDir = normalise $ refract incoming normal eta
          let offsetToInterior = madd intersectionPoint refractionDir surfaceEpsilon
          let !transmittance = transmit $ material obj
          let refractRay = rayWithDirection offsetToInterior refractionDir (refractionRayLength renderContext)
          let (refraction, irrCache''') = if transmittance > 0 
                                          then runState (traceRay renderContext photonMap refractRay (limit - 1) viewDir (indexOfRefraction $ material obj) accumulatedReflectivity) irrCache''
                                          else (colBlack, irrCache'')
          put irrCache'''

          -- Final colour combine
          return $! (surfaceShading + (reflection Colour.<*> shine) + (refraction Colour.<*> transmittance))
              where
                enteringObject !incoming !normal = incoming `dot3` normal > 0

-- This function converts a pixel co-ordinate to a direction of the ray
makeRayDirection :: Int -> Int -> Camera -> (Int, Int) -> Vector
makeRayDirection !renderWidth !renderHeight !camera (x, y) =
    let x' = (fromIntegral x / fromIntegral renderWidth) * 2.0 - 1.0
        y' = (fromIntegral y / fromIntegral renderHeight) * 2.0 - 1.0
        fov = 0.5 * fieldOfView camera
        fovX = tan (degreesToRadians fov)
        fovY = -tan (degreesToRadians fov)
        aspectRatio = fromIntegral renderWidth / fromIntegral renderHeight
        !dirX = fovX * x'
        !dirY = fovY * (-y') / aspectRatio
        rayDir = normalise (Vector dirX dirY 1 0)
    in normalise $ transformVector (worldToCamera camera) rayDir

-- Trace a list of distributed samples with tail recursion
traceDistributedSample :: RenderContext -> Colour -> [Position] -> Maybe PhotonMap -> (Position, Direction) -> Double -> RayTraceState
traceDistributedSample renderContext !acc (x:xs) photonMap !eyeViewDir !sampleWeighting = 
    do
      irrCache <- get
      let !dofFocalDistance = depthOfFieldFocalDistance renderContext
      let jitteredRayPosition jitter = fst eyeViewDir + jitter
      let jitteredRayDirection jitter = normalise $ madd jitter (snd eyeViewDir) dofFocalDistance
      let (sampleColour, irrCache') = runState (traceRay renderContext photonMap (rayWithDirection (jitteredRayPosition x) (jitteredRayDirection x) 100000.0) (maximumRayDepth renderContext) (snd eyeViewDir) 1 1) irrCache
      let result = (sampleColour Colour.<*> sampleWeighting) + acc
      put irrCache'
      let (col, irrCache'') = runState (traceDistributedSample renderContext result xs photonMap eyeViewDir sampleWeighting) irrCache'
      put irrCache''
      return $! col
traceDistributedSample _ !acc [] _ _ _ = return $! acc

-- Need to remove hard coded constants of 8 here
-- This traces for a given pixel (x, y)
tracePixel :: RenderContext -> Position -> Maybe PhotonMap -> Direction -> RayTraceState
tracePixel renderContext !eye photonMap !viewDirection = do
  irrCache <- get
  let !distributedPositions = generatePointsOnSphere (numDistribSamples renderContext) (rayOriginDistribution renderContext) 12345
  let (!pixelColour, irrCache') = runState (traceDistributedSample renderContext colBlack distributedPositions photonMap (eye, viewDirection) (1.0 / (fromIntegral . numDistribSamples $ renderContext))) irrCache
  put irrCache'
  return $! pixelColour

-- Generate a list of colours which contains a raytraced image. In parallel
rayTraceImage :: RenderContext -> Camera -> Int -> Int -> Maybe PhotonMap -> [Colour]
rayTraceImage renderContext camera renderWidth renderHeight photonMap = tracePixelPassingState rayDirections irrCache `using` parListChunk 256 rdeepseq
    where !rayDirections = [makeRayDirection renderWidth renderHeight camera (x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
          !eyePosition = Camera.position camera
          irrCache = initialiseCache (sceneGraph renderContext)
          -- This function is the equivalent to map, but it passes the ending state of one invocation to the next invocation
          tracePixelPassingState (x:xs) st = result : tracePixelPassingState xs st'
              where
                (!result, !st') = runState (tracePixel renderContext eyePosition photonMap x) st
          tracePixelPassingState [] _ = []
