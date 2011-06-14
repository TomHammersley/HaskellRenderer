-- This is a module for constructing bounding volume hierarchies using an octree approach

module Octree(generateSceneGraphUsingOctree, generateOctreeBoxList) where

import Vector
import Primitive
import BoundingBox

-- Generate a scene graph using an octree. Refactor this to just be an octree later
generateOctreeBoxList :: AABB -> [AABB]
generateOctreeBoxList (boxMin, boxMax) = 
    [
     (Vector (vecX boxMin) (vecY boxMin) (vecZ boxMin) 1, Vector (vecX centre) (vecY centre) (vecZ centre) 1),
     (Vector (vecX centre) (vecY boxMin) (vecZ boxMin) 1, Vector (vecX boxMax) (vecY centre) (vecZ centre) 1),
     (Vector (vecX boxMin) (vecY centre) (vecZ boxMin) 1, Vector (vecX centre) (vecY boxMax) (vecZ centre) 1),
     (Vector (vecX centre) (vecY centre) (vecZ boxMin) 1, Vector (vecX boxMax) (vecY boxMax) (vecZ centre) 1),

     (Vector (vecX boxMin) (vecY boxMin) (vecZ centre) 1, Vector (vecX centre) (vecY centre) (vecZ boxMax) 1),
     (Vector (vecX centre) (vecY boxMin) (vecZ centre) 1, Vector (vecX boxMax) (vecY centre) (vecZ boxMax) 1),
     (Vector (vecX boxMin) (vecY centre) (vecZ centre) 1, Vector (vecX centre) (vecY boxMax) (vecZ boxMax) 1),
     (Vector (vecX centre) (vecY centre) (vecZ centre) 1, Vector (vecX boxMax) (vecY boxMax) (vecZ boxMax) 1)
    ]
    where
      centre = (boxMin + boxMax) <*> 0.5

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
