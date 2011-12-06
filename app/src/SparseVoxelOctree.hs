-- The sparse voxel octree data structure
{-# LANGUAGE BangPatterns #-}

module SparseVoxelOctree(build, 
                         SparseOctree, 
                         intersect, 
                         boundingRadius, 
                         boundingBox) where

import Octree
import BoundingBox
import Ray
import Matrix

data SparseOctree = SparseOctreeDummy
                  | SparseOctreeNode !AABB [SparseOctree]
                  | SparseOctreeLeaf !AABB Double deriving (Eq)

instance Show SparseOctree where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: Int -> SparseOctree -> String
display level (SparseOctreeDummy) = take level tabs ++ "[Dummy]\n"
display level (SparseOctreeNode box children) = take level tabs ++ "[Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (SparseOctreeLeaf box value) = take level tabs ++ "[Leaf] box=" ++ show box ++ " value=" ++ show value ++ "\n"

-- Build a sparse voxel octree for a data set
build :: (AABB -> Double) -> AABB -> Int -> SparseOctree
build func box = build' func box 0

build' :: (AABB -> Double) -> AABB -> Int -> Int -> SparseOctree
build' func box depth maxDepth
  | depth == maxDepth = SparseOctreeLeaf box (func box)
  | otherwise = SparseOctreeNode box (map buildNonEmptyNodes subBoxList)
    where
      subBoxList = splitBoxIntoOctreeChildren box
      buildNonEmptyNodes childBox = if f > 0 then build' func childBox (depth + 1) maxDepth
                                    else SparseOctreeDummy
        where
          f = func childBox
                                         
-- Intersect with a ray
intersect :: Ray -> SparseOctree -> Maybe (Double, Int)
intersect _ SparseOctreeDummy = Nothing
intersect ray (SparseOctreeNode box children) = case intersectRayAABB box ray identity of
  Nothing -> Nothing
  Just dist -> undefined
intersect ray (SparseOctreeLeaf box _) = case intersectRayAABB box ray identity of
  Nothing -> Nothing
  Just dist -> Just (dist, 0)

boundingRadius :: SparseOctree -> Double
boundingRadius SparseOctreeDummy = 0
boundingRadius (SparseOctreeNode box _) = boundingBoxRadius box
boundingRadius (SparseOctreeLeaf box _) = boundingBoxRadius box

boundingBox :: SparseOctree -> AABB
boundingBox SparseOctreeDummy = error "Invalid SVO"
boundingBox (SparseOctreeNode box _) = box
boundingBox (SparseOctreeLeaf box _) = box
