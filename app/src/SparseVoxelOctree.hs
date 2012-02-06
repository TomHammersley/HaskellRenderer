-- The sparse voxel octree data structure
{-# LANGUAGE BangPatterns #-}

module SparseVoxelOctree(build, 
                         SparseOctree, 
                         closestIntersect, 
                         anyIntersect, 
                         boundingRadius, 
                         boundingBox,
                         enumerateLeafBoxes) where

import Octree
import BoundingBox
import Ray
import Vector
import PolymorphicNum

data SparseOctree = SparseOctreeDummy
                  | SparseOctreeNode AABB [SparseOctree]
                  | SparseOctreeLeaf AABB Double TangentSpace deriving (Eq)

instance Show SparseOctree where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: Int -> SparseOctree -> String
display level (SparseOctreeDummy) = take level tabs ++ show level ++ " [Dummy]\n"
display level (SparseOctreeNode box children) = take level tabs ++ show level ++ " [Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (SparseOctreeLeaf box value tanSpace) = take level tabs ++ show level ++ " [Leaf] box=" ++ show box ++ " value=" ++ show value ++ " tanSpace=" ++ show tanSpace ++ "\n"

-- Work out a tangent space for an SVO leaf box
calculateTangentSpace :: (Position -> Double) -> AABB -> TangentSpace
calculateTangentSpace f box = constructTangentSpace finalNormal
  where
    centre = boundingBoxCentre box
    normals = accumulateNormal (boundingBoxVertices box)
    finalNormal | null normals = error "Error - we should not be trying to form a tangent space for an empty leaf!"
                | otherwise = normalise $ foldr1 (<+>) normals </> ((fromIntegral $ length normals) :: Double)
    -- This little function makes a list of "solid matter to point away from". Ie, if there is some solid matter on our right, the normal should point away from it
    -- Empty spaces are simply omitted or not added to the list. We should end up with a non-empty list of normals to average out
    accumulateNormal (x:xs) | f x > 0 = normalise (centre <-> x) : accumulateNormal xs
                            | otherwise = accumulateNormal xs
    accumulateNormal [] = []

-- Build a sparse voxel octree for a data set
build :: (AABB -> Double) -> (Position -> Double) -> AABB -> Int -> Double -> SparseOctree
build = build' 0
  where
    build' depth func func2 box maxDepth minDimension
      | func box <= 0 = SparseOctreeDummy -- Really this is a soft error. But, the user might specify an invalid input...
      | depth == maxDepth || boundingBoxRadius box <= minDimension = SparseOctreeLeaf box (func box) (calculateTangentSpace func2 box)
      | otherwise = SparseOctreeNode box (concatMap buildNonEmptyNodes subBoxList)
      where
        subBoxList = splitBoxIntoOctreeChildren box
        buildNonEmptyNodes childBox 
          | func childBox > 0 = [build' (depth + 1) func func2 childBox maxDepth minDimension]
          | otherwise = []
                                         
-- Find the closest Maybe intersection
nearestIntersection :: Maybe (Double, TangentSpace) -> Maybe (Double, TangentSpace) -> Maybe (Double, TangentSpace)
nearestIntersection Nothing Nothing = Nothing
nearestIntersection Nothing x@(Just _) = x
nearestIntersection y@(Just _) Nothing = y
nearestIntersection x@(Just (d1, _)) y@(Just (d2, _)) | d1 < d2 = x 
                                                      | otherwise = y
                                                                    
-- Work out if a further refinement of given intersection is likely to result in a feature hardly visible in the image
-- TODO: Make this work on distance to eye, not distance along ray
intersectionWorthSubdivision :: Double -> Double -> Double -> Bool
intersectionWorthSubdivision d scaler radius = radius * scaler / d >= 2

-- Intersect with a ray
closestIntersect :: Ray -> Int -> Int -> Double -> SparseOctree -> Maybe (Double, TangentSpace)
closestIntersect _ _ _ _ SparseOctreeDummy = Nothing
closestIntersect ray depth maxDepth lodScale (SparseOctreeNode box children) =
  case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                          Just (dist, dist2) -> if depth >= maxDepth || (not $ intersectionWorthSubdivision dist lodScale (boundingBoxRadius box))
                                                                then Just (dist, boundingBoxTangentSpace box (pointAlongRay ray dist))
                                                                else foldr1 nearestIntersection (map (closestIntersect (shortenRay ray dist2) (depth + 1) maxDepth lodScale) children)
closestIntersect ray _ _ _ (SparseOctreeLeaf box _ tanSpace) = case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                                                                                       Just (dist, _) -> Just (dist, tanSpace)

anyIntersect :: Ray -> Int -> Int -> Double -> SparseOctree -> Maybe (Double, TangentSpace)
anyIntersect _ _ _ _ SparseOctreeDummy = Nothing
anyIntersect ray depth maxDepth lodScale (SparseOctreeNode box children) =
  case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                          Just (dist, dist2) -> if depth >= maxDepth || (not $ intersectionWorthSubdivision dist lodScale (boundingBoxRadius box))
                                                                then Just (dist, boundingBoxTangentSpace box (pointAlongRay ray dist))
                                                                else traverseChildren children
                                            where
                                              traverseChildren [] = Nothing
                                              traverseChildren (x:xs) = case anyIntersect (shortenRay ray dist2) (depth + 1) maxDepth lodScale x of
                                                Nothing -> traverseChildren xs
                                                Just z -> Just z
anyIntersect ray _ _ _ (SparseOctreeLeaf box _ tanSpace) = case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                                                                                   Just (dist, _) -> Just (dist, tanSpace)

boundingRadius :: SparseOctree -> Double
boundingRadius SparseOctreeDummy = 0
boundingRadius (SparseOctreeNode box _) = boundingBoxRadius box
boundingRadius (SparseOctreeLeaf box _ _) = boundingBoxRadius box

boundingBox :: SparseOctree -> AABB
boundingBox SparseOctreeDummy = error "Invalid SVO"
boundingBox (SparseOctreeNode box _) = box
boundingBox (SparseOctreeLeaf box _ _) = box

-- Traverse the whole tree and make a list of bounding boxes for each leaf
enumerateLeafBoxes :: SparseOctree -> [AABB]
enumerateLeafBoxes = enumerateLeafBoxes' []
  where
    enumerateLeafBoxes' acc SparseOctreeDummy = acc
    enumerateLeafBoxes' acc (SparseOctreeNode _ children) = acc ++ concatMap (enumerateLeafBoxes' []) children
    enumerateLeafBoxes' acc (SparseOctreeLeaf box _ _) = box : acc
