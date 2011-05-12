-- Bounding box code

module BoundingBox where

import Vector

type AABB = (Vector, Vector)

boundingBoxUnion :: AABB -> AABB -> AABB
boundingBoxUnion (min1, max1) (min2, max2) = (Vector.min min1 min2, Vector.max max1 max2)

boundingBoxValid :: AABB -> Bool
boundingBoxValid (boxMin, boxMax) = (vecX boxMin) <= (vecX boxMax) &&
                                    (vecY boxMin) <= (vecY boxMax) &&
                                    (vecZ boxMin) <= (vecZ boxMax)

boundingBoxOverlaps :: AABB -> AABB -> Bool
boundingBoxOverlaps box1 box2 = (overlaps box1 box2) || (overlaps box2 box1)
    where
      overlaps (min1, max1) (min2, max2) = (vecX min1) <= (vecX max2) && (vecX max1 >= vecX min2) &&
                                           (vecY min1) <= (vecY max2) && (vecY max1 >= vecY min2) &&
                                           (vecZ min1) <= (vecZ max2) && (vecZ max1 >= vecZ min2)

-- Enlarge a bounding box to include a point
enlargeBoundingBox :: Position -> AABB -> AABB
enlargeBoundingBox pos(boxMin, boxMax) = (Vector.min boxMin pos, Vector.max boxMax pos)

-- This is a dummy box that is used initially. If anything is intersected with it, it becomes valid. Else it is an invalid box that can be tested for
initialInvalidBox :: AABB
initialInvalidBox = (Vector bigNumber bigNumber bigNumber 1, Vector smallNumber smallNumber smallNumber 1)
    where
      bigNumber = 10000000
      smallNumber = -10000000

-- These functions are useful for finding the greatest or smallest part of a box relative to a normal
selectMinBoxComponent :: (Vector -> Float) -> Vector -> AABB -> Float
selectMinBoxComponent f norm (boxMin, boxMax) = if (f norm) > 0 then (f boxMin) else (f boxMax)

selectMaxBoxComponent :: (Vector -> Float) -> Vector -> AABB -> Float
selectMaxBoxComponent f norm (boxMin, boxMax) = if (f norm) > 0 then (f boxMax) else (f boxMin)
