-- The module where all the tracing actually happens
{-# LANGUAGE BangPatterns #-}

module RayTrace (rayTraceImage, findNearestIntersection, findAnyIntersection, RenderContext(RenderContext)) where

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
import {-# SOURCE #-} PhotonMap (PhotonMap, PhotonMapContext, irradiance)

data RenderContext = RenderContext {
      numDistribSamples :: Int,
      sceneGraph :: SceneGraph,
      lights :: [Light],
      maximumRayDepth :: Int,
      reflectionRayLength :: Float,
      refractionRayLength :: Float,
      photonMapContext :: PhotonMapContext }

-- Intersect a ray against a sphere tree
intersectSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Float, Int) -> Maybe (Object, Float, Int)
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
intersectObjectList :: [Object] -> Ray -> Maybe (Object, Float, Int) -> Maybe (Object, Float, Int)
intersectObjectList !(obj:objs) !ray !currentHit = intersectObjectList objs newRay thisResult
    where
      (!thisResult, !newRay) = case primitiveClosestIntersect (primitive obj) ray obj of
                                 Nothing -> (currentHit, ray)
                                 Just (objHitDistance, objHitId) -> (Just (obj, objHitDistance, objHitId), shortenRay ray objHitDistance)
intersectObjectList [] _ !currentHit = currentHit

-- Find the nearest intersection along a line
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Float, Int)
findNearestIntersection sceneGraph' !ray = case intersectObjectList (infiniteObjects sceneGraph') ray Nothing of
                                             Just (obj, dist, objId) -> intersectSphereTree [root sceneGraph'] (shortenRay ray dist) (Just (obj, dist, objId))
                                             Nothing -> intersectSphereTree [root sceneGraph'] ray Nothing

-- Intersect a ray against a scene graph. Return first (ie, any) hit without finding the closest
findAnyIntersectionSphereTree :: [SphereTreeNode] -> Ray -> Maybe (Object, Float)
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
findAnyIntersectionObjectList :: [Object] -> Ray -> Maybe (Object, Float)
findAnyIntersectionObjectList (obj:objs) !ray = case primitiveAnyIntersect (primitive obj) ray obj of
                                                  -- Didn't hit he object. Retain the current hit, but offer up the children of the node as we hit the bounding volume
                                                  Nothing -> findAnyIntersectionObjectList objs ray
                                                  -- We did hit this object. Update the intersection, and continue with the bounding volume's children
                                                  Just (objHitDistance, _) -> Just (obj, objHitDistance)
findAnyIntersectionObjectList [] _ = Nothing

findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Float)
findAnyIntersection sceneGraph' !ray = case findAnyIntersectionObjectList (infiniteObjects sceneGraph') ray of
                                        Nothing -> findAnyIntersectionSphereTree [root sceneGraph'] ray
                                        Just x -> Just x

-- Default background colour to return when we can't match anything
defaultColour :: Direction -> Colour
defaultColour _ = colBlue

-- Are we entering or leaving a material?
enteringObject :: Vector -> Vector -> Bool 
enteringObject !incoming !normal = incoming `dot3` normal > 0

-- Accumulate the contributions of the lights
accumulateLight :: [Light] -> Colour -> SceneGraph -> (Position, TangentSpace) -> Material -> Vector -> Colour
accumulateLight (x:xs) !acc sceneGraph' !posTanSpace !objMaterial !viewDirection = let result = acc + applyLight sceneGraph' posTanSpace objMaterial viewDirection x
                                                                                   in seq result (accumulateLight xs result sceneGraph' posTanSpace objMaterial viewDirection)
accumulateLight [] !acc _ _ _ _ = acc

-- Perform a full trace of a ray
traceRay :: RenderContext -> PhotonMap -> Ray -> Int -> Direction -> Float -> Float -> Colour

-- Special case for lowest level of recursion (theoretically this should not get hit)
traceRay _ _ _ 0 _ _ _ = error "Should not hit this codepath"

-- Special case for penultimate level - we're not allowed to spawn rays here
traceRay renderContext photonMap !ray 1 !viewDir _ _ = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> defaultColour $ direction ray
        Just (obj, intersectionDistance, hitId) -> surfaceIrradiance + accumulateLight (lights renderContext) colBlack (sceneGraph renderContext) (intersectionPoint, tanSpace) (material obj) viewDir
            where
                -- Evaluate surface-location specific things such as shader results
                !tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
                !intersectionPoint = pointAlongRay ray intersectionDistance
                !surfaceIrradiance = irradiance photonMap (intersectionPoint, tanSpace) (photonMapContext renderContext) (material obj)

-- General case
traceRay renderContext photonMap !ray !limit !viewDir !currentIOR !accumulatedReflectivity = 
    case findNearestIntersection (sceneGraph renderContext) ray of
        Nothing -> defaultColour $ direction ray
        Just (obj, intersectionDistance, hitId) -> surfaceIrradiance + surfaceShading + reflection + refraction
            where
                -- Evaluate surface-location specific things such as shader results
                !intersectionPoint = pointAlongRay ray intersectionDistance
                !tanSpace = primitiveTangentSpace (primitive obj) hitId intersectionPoint obj
                !normal = thr tanSpace
                !incoming = Vector.negate $ direction ray
                !surfaceShading = accumulateLight (lights renderContext) colBlack (sceneGraph renderContext) (intersectionPoint, tanSpace) (material obj) viewDir
                !surfaceIrradiance = irradiance photonMap (intersectionPoint, tanSpace) (photonMapContext renderContext) (material obj)

                -- Reflection specific code
                offsetToExterior = madd intersectionPoint normal surfaceEpsilon
                reflectionDir = normalise $ reflect incoming normal
                !shine = reflectivity $ material obj
                reflection 
                    | shine > 0 && (accumulatedReflectivity * shine) > 0.03 = traceRay renderContext photonMap (rayWithDirection offsetToExterior reflectionDir (reflectionRayLength renderContext)) (limit - 1) viewDir currentIOR (accumulatedReflectivity * shine) `colourMul` shine
                    | otherwise = colBlack

                -- Refraction specific
                eta = if enteringObject incoming normal
                      then currentIOR / indexOfRefraction (material obj) 
                      else indexOfRefraction (material obj) / currentIOR
                refractionDir = normalise $ refract incoming normal eta
                offsetToInterior = madd intersectionPoint refractionDir surfaceEpsilon
                !transmittance = transmit $ material obj
                refraction 
                    | transmittance > 0 = traceRay renderContext photonMap (rayWithDirection offsetToInterior refractionDir (refractionRayLength renderContext)) (limit - 1) viewDir (indexOfRefraction $ material obj) accumulatedReflectivity `colourMul` transmittance
                    | otherwise = colBlack

-- This function converts a pixel co-ordinate to a direction of the ray
makeRayDirection :: Int -> Int -> Camera -> (Int, Int) -> Vector
makeRayDirection !renderWidth !renderHeight !camera (x, y) =
    let x' = (fromIntegral x / fromIntegral renderWidth) * 2.0 - 1.0
        y' = (fromIntegral y / fromIntegral renderHeight) * 2.0 - 1.0
        fov = 0.5 * fieldOfView camera
        fovX = tan (degreesToRadians fov)
        fovY = -tan (degreesToRadians fov)
        aspectRatio = fromIntegral renderWidth / fromIntegral renderHeight
        rayDir = normalise (Vector (fovX * x') (fovY * (-y') / aspectRatio) 1 0)
    in normalise $ transformVector (worldToCamera camera) rayDir

-- What is the radius of the sphere over which we want to distribute ray origin samples?
rayOriginDistribution :: Float
rayOriginDistribution = 0.5

depthOfFieldFocalDistance :: Float
depthOfFieldFocalDistance = 400.0

-- Trace a list of distributed samples with tail recursion
traceDistributedSample :: RenderContext -> Colour -> [Position] -> PhotonMap -> (Position, Direction) -> Float -> Colour
traceDistributedSample renderContext !acc (x:xs) photonMap !eyeViewDir !sampleWeighting = 
    let result = sampleColour + acc
    in seq result (traceDistributedSample renderContext result xs photonMap eyeViewDir sampleWeighting)
    where
      sampleColour = traceRay renderContext photonMap (rayWithDirection (jitteredRayPosition x) (jitteredRayDirection x) 100000.0) (maximumRayDepth renderContext) (snd eyeViewDir) 1 1 `colourMul` sampleWeighting
      jitteredRayPosition jitter = fst eyeViewDir + jitter
      jitteredRayDirection jitter = normalise $ madd jitter (snd eyeViewDir) depthOfFieldFocalDistance
traceDistributedSample _ !acc [] _ _ _ = acc

-- Need to remove hard coded constants of 8 here
-- This traces for a given pixel (x, y)
tracePixel :: RenderContext -> Position -> PhotonMap -> Direction -> Colour
tracePixel renderContext !eye photonMap !viewDirection = invGammaCorrect pixelColour
    where 
      !distributedPositions = generatePointsOnSphere (numDistribSamples renderContext) rayOriginDistribution
      !pixelColour = seq distributedPositions $ traceDistributedSample renderContext colBlack distributedPositions photonMap (eye, viewDirection) (1.0 / (fromIntegral . numDistribSamples $ renderContext))

-- Generate a list of colours which contains a raytraced image. In parallel
rayTraceImage :: RenderContext -> Camera -> Int -> Int -> PhotonMap -> [Colour]
rayTraceImage renderContext camera renderWidth renderHeight photonMap = map (clamp . tracePixel renderContext eyePosition photonMap) rayDirections `using` parListChunk 256 rseq
    where !rayDirections = [makeRayDirection renderWidth renderHeight camera (x, y) | y <- [0..(renderHeight - 1)], x <- [0..(renderWidth - 1)]]
          !eyePosition = Camera.position camera
