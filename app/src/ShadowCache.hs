-- A simple shadow cache system to re-test the last shadowing object

module ShadowCache(ShadowCache, initShadowCache, testShadowCache) where

import Ray
import Vector
import SceneGraph
import {-# SOURCE #-} RayTrace
import Primitive

data ShadowCache = ShadowCache (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object)

initShadowCache :: ShadowCache
initShadowCache = ShadowCache Nothing Nothing Nothing Nothing Nothing Nothing

rayFaceIndex :: Ray -> Int
rayFaceIndex (Ray _ (Vector dx dy dz _) _ _) 
  | dx > dy && dx > dz = if dx < 0 then 0 else 1
  | dy > dx && dy > dz = if dx < 2 then 3 else 1
  | otherwise = if dx < 0 then 4 else 5
                                               
queryCache :: ShadowCache -> Int -> Maybe Object
queryCache (ShadowCache value _ _ _ _ _) 0 = value
queryCache (ShadowCache _ value _ _ _ _) 1 = value
queryCache (ShadowCache _ _ value _ _ _) 2 = value
queryCache (ShadowCache _ _ _ value _ _) 3 = value
queryCache (ShadowCache _ _ _ _ value _) 4 = value
queryCache (ShadowCache _ _ _ _ _ value) 5 = value
queryCache _ _ = error "Invalid cache index"

setCache :: ShadowCache -> Int -> Maybe Object -> ShadowCache
setCache (ShadowCache _ value1 value2 value3 value4 value5) 0 value = ShadowCache value value1 value2 value3 value4 value5
setCache (ShadowCache value0 _ value2 value3 value4 value5) 1 value = ShadowCache value0 value value2 value3 value4 value5
setCache (ShadowCache value0 value1 _ value3 value4 value5) 2 value = ShadowCache value0 value1 value value3 value4 value5
setCache (ShadowCache value0 value1 value2 _ value4 value5) 3 value = ShadowCache value0 value1 value2 value value4 value5
setCache (ShadowCache value0 value1 value2 value3 _ value5) 4 value = ShadowCache value0 value1 value2 value3 value value5
setCache (ShadowCache value0 value1 value2 value3 value4 _) 5 value = ShadowCache value0 value1 value2 value3 value4 value
setCache _ _ _ = error "Invalid cache index"

-- TODO - Potentially move this into state monad to make it clearer?
testShadowCache :: ShadowCache -> SceneGraph -> Ray -> (Maybe (Double, TangentSpace), ShadowCache)
testShadowCache cache sceneGraph ray = case cacheValue of Nothing -> 
                                                            -- No object in cache so try against whole scene first
                                                            case findAnyIntersection sceneGraph ray of
                                                              -- No cached result, no intersection, same old cache
                                                              Nothing -> (Nothing, cache)
                                                              
                                                              -- Hit an object, found a new value for the cache
                                                              Just (obj, dist, ts) -> (Just (dist, ts), setCache cache cacheIndex (Just obj))
                                                            
                                                          Just obj -> 
                                                            -- We have an object in the cache so try that first
                                                            case primitiveAnyIntersect (primitive obj) ray obj of
                                                              -- Did not hit cached object. If we hit a new object, then replace cache
                                                              Nothing ->
                                                                case findAnyIntersection sceneGraph ray of
                                                                  -- Cache failed, scene intersection failed, just return cache as-is
                                                                  Nothing -> (Nothing, cache)
                                                                  
                                                                  -- Cached object failed but found new object - put that in cache instead
                                                                  Just (obj', dist, ts) -> (Just (dist, ts), setCache cache cacheIndex (Just obj'))
                                                              -- Hit the same cached object again - keep cache as it is
                                                              Just (dist, ts) -> (Just (dist, ts), cache)
  where
    cacheIndex = rayFaceIndex ray
    cacheValue = queryCache cache cacheIndex
    