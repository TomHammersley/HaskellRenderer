-- This is a module for constructing bounding volume hierarchies using a kdtree
{-# LANGUAGE MagicHash #-}

module KDTree(generateSceneGraphUsingKDTree, makeSplittingPlane, degenerateSplitList, findSplittingPlane) where
import Vector
import Primitive
import Data.List
import BoundingBox

-- This stuff is object specific 

-- What side of a plane is an object on?
onPositiveSide :: (Vector, Double) -> Object -> Bool
onPositiveSide (planeNormal, planeDist) obj = planeDist + (planeNormal `dot3` objBoxCentre) > 0.01
    where
      Just (boxMin, boxMax) = primitiveBoundingBox (primitive obj) obj
      objBoxCentre = (boxMin + boxMax) <*> 0.5

-- This stuff is generic

-- Generate a plane to split the objects along
makeSplittingPlane :: AABB -> Int -> (Vector, Double)
makeSplittingPlane (boxMin, boxMax) buildCycle = case nthLargestAxis (boxMax - boxMin) buildCycle of
                                                   0 -> (xaxis, -(vecX midPoint))
                                                   1 -> (yaxis, -(vecY midPoint))
                                                   2 -> (zaxis, -(vecZ midPoint))
                                                   _ -> error "Undefined value"
    where
      midPoint = (boxMin + boxMax) <*> 0.5

-- Find a working splitting plane
findSplittingPlane :: AABB -> Int -> [t] -> ((Vector, Double) -> t -> Bool) -> Maybe (Vector, Double) 
findSplittingPlane box buildCycle objs partitionFunc
    | buildCycle > 2 = Nothing
    | otherwise = if length leftObjects > 0 && length rightObjects > 0 
                  then Just candidateSplittingPlane
                  else findSplittingPlane box (buildCycle + 1) objs partitionFunc
                      where
                        candidateSplittingPlane = makeSplittingPlane box buildCycle
                        (leftObjects, rightObjects) = partition (partitionFunc candidateSplittingPlane) objs

-- We use this dysfunctional strategy where all our smarter ideas run out
degenerateSplitList :: (Eq t) => [t] -> ([t], [t])
degenerateSplitList objs = ([x | x <- objs, case x `elemIndex` objs of
                                              Just index -> odd index
                                              Nothing -> False], 
                            [x | x <- objs, case x `elemIndex` objs of
                                              Just index -> even index
                                              Nothing -> False])

-- Make children using a kd tree
generateSceneGraphUsingKDTree :: [Object] -> [[Object]]
generateSceneGraphUsingKDTree objs = [leftObjects, rightObjects]
    where
      objBox = objectListBoundingBox objs
      (leftObjects, rightObjects) = case findSplittingPlane objBox 0 objs onPositiveSide of
                                      Nothing -> degenerateSplitList objs
                                      Just splittingPlane -> partition (onPositiveSide splittingPlane) objs
