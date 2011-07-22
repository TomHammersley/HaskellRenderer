-- The graph structure holding the scene

module SceneGraph (buildSceneGraph, SphereTreeNode(boundingRadius, boundingCentre, object, children), SceneGraph(root, infiniteObjects, finiteBox)) where

import Primitive
import Vector
import BoundingBox

data SphereTreeNode = SphereTreeNode { object :: Maybe Object, children :: [SphereTreeNode], boundingRadius :: !Float, boundingCentre :: !Vector } deriving (Show, Read)
data SceneGraph = SceneGraph { root :: SphereTreeNode, infiniteObjects :: [Object], finiteBox :: AABB } deriving (Show, Read)

-- Find the mean of a collection of objects
calculateMeanPosition' :: [Object] -> Vector -> Vector
calculateMeanPosition' (obj : objects) acc = calculateMeanPosition' objects acc + getCentre obj
calculateMeanPosition' [] acc = acc

calculateMeanPosition :: [Object] -> Vector
calculateMeanPosition objects = setWTo1 (calculateMeanPosition' objects zeroVector </> fromIntegral (length objects))

-- Find the overall bounding radius of a list of objects
calculateBoundingRadius :: [Object] -> Vector -> Float
calculateBoundingRadius objs centre = foldr (Prelude.max . (\obj -> primitiveBoundingRadius (primitive obj) (transform obj) centre)) 0 objs

-- Build up a sphere tree
buildSphereTree :: ([Object] -> [[Object]]) -> [Object] -> SphereTreeNode
buildSphereTree _ (obj : []) = SphereTreeNode (Just obj) [] nodeRadius nodeCentre
    where
      nodeCentre = calculateMeanPosition [obj]
      nodeRadius = calculateBoundingRadius [obj] nodeCentre
buildSphereTree builder (obj:objs)
    | length (obj:objs) == 1 = error "Should have been handled by a different pattern"
    | null (obj:objs) = error "Should not have zero objects"
    | otherwise = SphereTreeNode Nothing nodeChildren nodeRadius nodeCentre
    where
      nodeCentre = calculateMeanPosition (obj:objs)
      nodeRadius = calculateBoundingRadius (obj:objs) nodeCentre
      nodeChildren = map (buildSphereTree builder) (builder (obj:objs))
buildSphereTree _ [] = error "Should not hit this pattern for buildSphereTree" 

-- Build a scene graph
buildSceneGraph :: [Object] -> ([Object] -> [[Object]]) -> SceneGraph
buildSceneGraph objs buildFunction = SceneGraph (buildSphereTree buildFunction nonInfiniteObjects) infiniteObjs (objectListBoundingBox nonInfiniteObjects)
    where
      nonInfiniteObjects = filter (not . infinitePrimitive . primitive) objs
      infiniteObjs = filter (infinitePrimitive . primitive) objs
