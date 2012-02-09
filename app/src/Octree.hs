-- This is a module for constructing bounding volume hierarchies using an octree approach
{-# LANGUAGE BangPatterns #-}

module Octree(generateSceneGraphUsingOctree, splitBoxIntoOctreeChildren, octreeChildBox, Octree(OctreeNode, OctreeLeaf, OctreeDummy), create, Octree.insert, gather) where

import Vector
import {-# SOURCE #-} Primitive
import BoundingBox
import Misc

data Octree a = OctreeDummy !AABB
               | OctreeNode !AABB [Octree a]
               | OctreeLeaf !AABB !(Vector, a) deriving (Eq)

instance Show a => Show (Octree a) where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: (Show a) => Int -> Octree a -> String
display level (OctreeDummy box) = take level tabs ++ "[Dummy] box=" ++ show box ++ "\n"
display level (OctreeNode box children) = take level tabs ++ "[Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (OctreeLeaf box (pos, value)) = take level tabs ++ "[Leaf] box=" ++ show box ++ " pos=" ++ show pos ++ " value=" ++ show value ++ "\n"

create :: AABB -> Octree a
create box = OctreeNode box $ map OctreeDummy (splitBoxIntoOctreeChildren box)

-- Insert into an octree
insert :: Vector -> a -> Octree a -> Octree a
insert pos a oct = fst $ insert' pos oct (Just a)

insert' :: Vector -> Octree a -> Maybe a -> (Octree a, Maybe a)
insert' pos oct@(OctreeDummy box) state = case state of
  -- If we have been passed some state then attempt to consume it
  Just value -> if box `contains` pos
                then (OctreeLeaf box (pos, value), Nothing)
                else (oct, state)
  _ -> (oct, state)

insert' pos oct@(OctreeNode box nodeChildren) state = if box `contains` pos
                                                      then let (nodeChildren', state') = mapS (insert' pos) nodeChildren state 
                                                           in (OctreeNode box nodeChildren', state')
                                                       else (oct, state)

insert' pos oct@(OctreeLeaf box (pos', a')) state = if box `contains` pos 
                                                    then 
                                                      -- First up, we turn this leaf into a node with 8 children
                                                      -- Discard result of mapS - we assume it returns Nothing
                                                      -- Then, re-insert the original value into our nascent octree
                                                      let (!newChildren, _) = mapS (insert' pos) (map OctreeDummy (splitBoxIntoOctreeChildren box)) state
                                                          (!octTree', !state') = insert' pos' (OctreeNode box newChildren) (Just a')
                                                      in (octTree', state')
                                                    else (oct, state)

-- Gather data within a sphere from an octree
gather :: Position -> Double -> Octree a -> [(a, Double)]
gather pos r (OctreeNode box nodeChildren) = if overlapsSphere box pos r
                                             then concatMap (gather pos r) nodeChildren
                                             else []
gather pos r (OctreeLeaf _ (pos', a))
    | dSq <= r * r = [(a, dSq)]
    | otherwise = []
    where dSq = pos `distanceSq` pos'
gather _ _ (OctreeDummy _) = []

-- Generate a scene graph using an octree. Refactor this to just be an octree later
splitBoxIntoOctreeChildren :: AABB -> [AABB]
splitBoxIntoOctreeChildren (Vector xmin ymin zmin _, Vector xmax ymax zmax _) =
    [
     (Vector xmin ymin zmin 1, Vector centreX centreY centreZ 1),
     (Vector centreX ymin zmin 1, Vector xmax centreY centreZ 1),
     (Vector xmin centreY zmin 1, Vector centreX ymax centreZ 1),
     (Vector centreX centreY zmin 1, Vector xmax ymax centreZ 1),

     (Vector xmin ymin centreZ 1, Vector centreX centreY zmax 1),
     (Vector centreX ymin centreZ 1, Vector xmax centreY zmax 1),
     (Vector xmin centreY centreZ 1, Vector centreX ymax zmax 1),
     (Vector centreX centreY centreZ 1, Vector xmax ymax zmax 1)
    ]
    where
      centreX = (xmin + xmax) * 0.5
      centreY = (ymin + ymax) * 0.5
      centreZ = (zmin + zmax) * 0.5

octreeChildBox :: AABB -> Int -> AABB
octreeChildBox (Vector !xmin !ymin !zmin _, Vector !xmax !ymax ! zmax _) index
  = case index of
    0 -> (Vector xmin ymin zmin 1, Vector centreX centreY centreZ 1)
    1 -> (Vector centreX ymin zmin 1, Vector xmax centreY centreZ 1)
    2 -> (Vector xmin centreY zmin 1, Vector centreX ymax centreZ 1)
    3 -> (Vector centreX centreY zmin 1, Vector xmax ymax centreZ 1)

    4 -> (Vector xmin ymin centreZ 1, Vector centreX centreY zmax 1)
    5 -> (Vector centreX ymin centreZ 1, Vector xmax centreY zmax 1)
    6 -> (Vector xmin centreY centreZ 1, Vector centreX ymax zmax 1)
    7 -> (Vector centreX centreY centreZ 1, Vector xmax ymax zmax 1)
    _ -> error "Invalid index"
    where
      !centreX = (xmin + xmax) * 0.5
      !centreY = (ymin + ymax) * 0.5
      !centreZ = (zmin + zmax) * 0.5

-- Octree code that's spilt out from other modules... this is scene graph specific helper code rather than self-contained octree stuff

-- Take a list of objects and split it into a list of objects that intersect a box, and those that don't
objectsIntersectingBox :: [Object] -> AABB -> ([Object], [Object])
objectsIntersectingBox objects box = objectsIntersectingBox' objects box ([], [])

objectsIntersectingBox' :: [Object] -> AABB -> ([Object], [Object]) -> ([Object], [Object])
objectsIntersectingBox' (obj:objs) box (currentHit, currentMiss) = if intersectsBox (primitive obj) (transform obj) box
                                                            then objectsIntersectingBox' objs box (obj : currentHit, currentMiss) 
                                                            else objectsIntersectingBox' objs box (currentHit, obj : currentMiss)
objectsIntersectingBox' [] _ (currentHit, currentMiss) = (currentHit, currentMiss)

-- Iterator function. Match up objects to this box, and then iterate with the remainder
assignObjectsToOctreeBoxes' :: [Object] -> [AABB] -> [[Object]] -> [[Object]]
assignObjectsToOctreeBoxes' objs (box:boxes) (x:xs) = assignObjectsToOctreeBoxes' remainingObjects boxes (matchedObjects : x : xs)
    where
      (matchedObjects, remainingObjects) = objectsIntersectingBox objs box
assignObjectsToOctreeBoxes' _ [] currentList = currentList
assignObjectsToOctreeBoxes' objs (box:boxes) [] = assignObjectsToOctreeBoxes' remainingObjects boxes [matchedObjects]
    where
      (matchedObjects, remainingObjects) = objectsIntersectingBox objs box

-- Generate the list of objects for each bounding box
assignObjectsToOctreeBoxes :: [Object] -> [AABB] -> [[Object]]
assignObjectsToOctreeBoxes objects boxes = assignObjectsToOctreeBoxes' objects boxes []

-- Make children using an octree algorithm
generateSceneGraphUsingOctree :: [Object] -> [[Object]]
generateSceneGraphUsingOctree (obj:objs) 
    | not (boundingBoxValid nodeBox) = error "Invalid bounding box"
    | otherwise = onlyPopulatedBoxes
    where
      nodeBox = objectListBoundingBox (obj:objs)
      octreeBoxes = splitBoxIntoOctreeChildren nodeBox
      objsPerOctreeBox = assignObjectsToOctreeBoxes (obj:objs) octreeBoxes
      onlyPopulatedBoxes = filter (\x -> length x > 0) objsPerOctreeBox
generateSceneGraphUsingOctree [] = []
