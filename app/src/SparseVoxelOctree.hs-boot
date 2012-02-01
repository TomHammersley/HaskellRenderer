-- The sparse voxel octree data structure

module SparseVoxelOctree(build, 
			 SparseOctree, 
			 closestIntersect,	
			 anyIntersect,
			 boundingRadius, 
			 boundingBox,
			 enumerateLeafBoxes) where

import BoundingBox
import Ray
import Vector

data SparseOctree = SparseOctreeDummy
                  | SparseOctreeNode !AABB [SparseOctree]
                  | SparseOctreeLeaf !AABB Double

instance Show SparseOctree
instance Eq SparseOctree

-- Build a sparse voxel octree for a data set
build :: (AABB -> Double) -> (Position -> Double) -> AABB -> Int -> Double -> SparseOctree

-- Intersect with a ray
closestIntersect :: Ray -> Int -> Int -> SparseOctree -> Maybe (Double, TangentSpace)
anyIntersect :: Ray -> Int -> Int -> SparseOctree -> Maybe (Double, TangentSpace)

-- The bounding radius
boundingRadius :: SparseOctree -> Double

-- The bounding box
boundingBox :: SparseOctree -> AABB

enumerateLeafBoxes :: SparseOctree -> [AABB]
