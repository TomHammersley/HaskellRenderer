module RayTrace (renderScene, findNearestIntersection, findAnyIntersection) where

import Vector
--import {-# SOURCE #-} Light
import {-# SOURCE #-} PhotonMap
import Primitive
import Colour
import Ray
--import Material
--import Matrix
import Camera
import SceneGraph
import RenderContext

--rayTraceImage :: RenderContext -> Camera -> Int -> Int -> PhotonMap -> [Colour]
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, TangentSpace)
findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Double, TangentSpace)
renderScene :: Maybe PhotonMap -> RenderContext -> Camera -> Int -> Int -> [Colour]
