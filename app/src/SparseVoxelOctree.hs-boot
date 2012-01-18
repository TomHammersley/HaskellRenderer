-- The sparse voxel octree data structure

module SparseVoxelOctree(build, 
			 SparseOctree, 
			 intersect, 
			 boundingRadius, 
			 boundingBox) where

import BoundingBox
import Ray

data SparseOctree = SparseOctreeDummy
                  | SparseOctreeNode !AABB [SparseOctree]
                  | SparseOctreeLeaf !AABB Double

instance Show SparseOctree
instance Eq SparseOctree

-- Build a sparse voxel octree for a data set
build :: (AABB -> Double) -> AABB -> Int -> SparseOctree

-- Intersect with a ray
intersect :: Ray -> Int -> Int -> SparseOctree -> Maybe (Double, Int)

-- The bounding radius
boundingRadius :: SparseOctree -> Double

-- The bounding box
boundingBox :: SparseOctree -> AABB
