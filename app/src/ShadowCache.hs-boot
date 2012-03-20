-- A simple shadow cache system to re-test the last shadowing object

module ShadowCache(ShadowCache, initShadowCache, testShadowCache) where

import Ray
import Vector
import SceneGraph
import Primitive

data ShadowCache = ShadowCache (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object) (Maybe Object)

initShadowCache :: ShadowCache
testShadowCache :: ShadowCache -> SceneGraph -> Ray -> (Maybe (Double, TangentSpace), ShadowCache)
