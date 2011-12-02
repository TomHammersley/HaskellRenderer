-- The sparse voxel octree data structure
{-# LANGUAGE BangPatterns #-}

module SparseVoxelOctree(build, SparseOctree) where

import Octree
import BoundingBox

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
                                         