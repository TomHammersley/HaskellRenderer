-- This is a render context, something that describes the general shared variables for rendering

module RenderContext where

import SceneGraph

data RenderMode = RayTrace | PhotonMapper | PathTracer

data RenderContext = RenderContext {
      numDistribSamples :: Int,
      sceneGraph :: SceneGraph,
      lights :: [Light],
      maximumRayDepth :: Int,
      reflectionRayLength :: Double,
      refractionRayLength :: Double,
      photonMapContext :: PhotonMapContext,
      rayOriginDistribution :: Double,
      depthOfFieldFocalDistance :: Double,
      renderMode :: RenderMode }
