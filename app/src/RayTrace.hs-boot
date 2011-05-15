module RayTrace (rayTraceImage, findNearestIntersection, findAnyIntersection, RenderContext(RenderContext)) where

import Vector
import Light
import Primitive
import Colour
import Ray
import Material
import Matrix
import Camera
import SceneGraph

data RenderContext = RenderContext {
      numDistribSamples :: Int,
      photonGatherDistance :: Float,
      sceneGraph :: SceneGraph,
      lights :: [Light],
      maximumRayDepth :: Int }

rayTraceImage :: RenderContext -> Camera -> Int -> Int -> PhotonMap -> [Colour]
findNearestIntersection :: SceneGraph -> Ray -> Maybe (Object, Float, Int)
findAnyIntersection :: SceneGraph -> Ray -> Maybe (Object, Float)
