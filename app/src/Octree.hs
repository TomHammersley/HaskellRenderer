-- This is a module for constructing bounding volume hierarchies using an octree approach
{-# LANGUAGE BangPatterns #-}

module Octree(generateSceneGraphUsingOctree, generateOctreeBoxList, OctTree(OctTreeNode, OctTreeLeaf, OctTreeDummy), create, insert, gather) where

import Vector
import Primitive
import BoundingBox
import Control.Parallel.Strategies

data OctTree a = OctTreeDummy AABB
               | OctTreeNode AABB [OctTree a]
               | OctTreeLeaf AABB (Vector, a) deriving (Read, Eq)

instance Show a => Show (OctTree a) where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: (Show a) => Int -> OctTree a -> String
display level (OctTreeDummy box) = take level tabs ++ "[Dummy] box=" ++ show box ++ "\n"
display level (OctTreeNode box children) = take level tabs ++ "[Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (OctTreeLeaf box (pos, value)) = take level tabs ++ "[Leaf] box=" ++ show box ++ " pos=" ++ show pos ++ " value=" ++ show value ++ "\n"

create :: AABB -> OctTree a
create box = OctTreeNode box $ map OctTreeDummy (generateOctreeBoxList box)

-- This performs a map, and passes through the state of the completed operation to the next recursion
-- Couldn't work out the equivalent using the state monad etc
mapS :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
mapS f xs state = mapS' f xs state []

mapS' :: (a -> s -> (b, s)) -> [a] -> s -> [b] -> ([b], s)
mapS' !f !(x:xs) !state !acc = seq (result, state') $ mapS' f xs state' (result : acc)
    where (!result, !state') = f x state `using` rseq
mapS' _ [] !state !acc = (acc, state)

-- Insert into an octree
insert :: Vector -> a -> OctTree a -> OctTree a
insert !pos a oct = fst $ insert' pos oct (Just a)

insert' :: Vector -> OctTree a -> Maybe a -> (OctTree a, Maybe a)
insert' !pos oct@(OctTreeDummy !box) !state = case state of
                                                -- If we have been passed some state then attempt to consume it
                                                Just !value -> if box `contains` pos
                                                               then (OctTreeLeaf box (pos, value), Nothing)
                                                               else (oct, state)
                                                _ -> (oct, state)
insert' !pos oct@(OctTreeNode !box !nodeChildren) !state = if box `contains` pos
                                                           then let (nodeChildren', state') = mapS (insert' pos) nodeChildren state 
                                                                in (OctTreeNode box nodeChildren', state')
                                                           else (oct, state)
insert' !pos (OctTreeLeaf !box (!pos', !a')) !state = (octTree', state')
    where
      -- First up, we turn this leaf into a node with 8 children
      (!newChildren, _) = mapS (insert' pos) (map OctTreeDummy (generateOctreeBoxList box)) state -- we're assuming that the octree insertion returns state of Nothing - else wtf happened?
      -- Now we re-insert the value that this leaf originally contained into the nascent octree
      (!octTree', !state') = insert' pos' (OctTreeNode box newChildren) (Just a')

-- Gather data within a sphere from an octree
gather :: Position -> Float -> OctTree a -> [(a, Float)]
gather !pos !r (OctTreeNode box nodeChildren) = if overlapsSphere box pos r
                                                then foldr ((++) . gather pos r) [] nodeChildren
                                                else []
gather !pos !r (OctTreeLeaf _ (!pos', a))
    | dSq <= r * r = [(a, dSq)]
    | otherwise = []
    where !dSq = pos `distanceSq` pos'
gather _ _ (OctTreeDummy _) = []

-- Generate a scene graph using an octree. Refactor this to just be an octree later
generateOctreeBoxList :: AABB -> [AABB]
generateOctreeBoxList (Vector !xmin !ymin !zmin _, Vector !xmax !ymax !zmax _) =
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
      octreeBoxes = generateOctreeBoxList nodeBox
      objsPerOctreeBox = assignObjectsToOctreeBoxes (obj:objs) octreeBoxes
      onlyPopulatedBoxes = filter (\x -> length x > 0) objsPerOctreeBox
generateSceneGraphUsingOctree [] = []
