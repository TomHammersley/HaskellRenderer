-- The sparse voxel octree data structure

module SparseVoxelOctree(build, 
                         SparseOctree, 
                         intersect, 
                         boundingRadius, 
                         boundingBox,
                         enumerateLeafBoxes) where

import Octree
import BoundingBox
import Ray

data SparseOctree = SparseOctreeDummy
                  | SparseOctreeNode !AABB [SparseOctree]
                  | SparseOctreeLeaf !AABB Double deriving (Eq)

instance Show SparseOctree where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: Int -> SparseOctree -> String
display level (SparseOctreeDummy) = take level tabs ++ show level ++ " [Dummy]\n"
display level (SparseOctreeNode box children) = take level tabs ++ show level ++ " [Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (SparseOctreeLeaf box value) = take level tabs ++ show level ++ " [Leaf] box=" ++ show box ++ " value=" ++ show value ++ "\n"

-- Build a sparse voxel octree for a data set
build :: (AABB -> Double) -> AABB -> Int -> SparseOctree
build = build' 0
  where
    build' depth func box maxDepth
      | func box <= 0 = SparseOctreeDummy -- Really this is a soft error. But, the user might specify an invalid input...
      | depth == maxDepth = SparseOctreeLeaf box (func box)
      | otherwise = SparseOctreeNode box (concatMap buildNonEmptyNodes subBoxList)
      where
        subBoxList = splitBoxIntoOctreeChildren box
        buildNonEmptyNodes childBox 
          | func childBox > 0 = [build' (depth + 1) func childBox maxDepth]
          | otherwise = []
                                         
-- Find the closest Maybe intersection
nearestIntersection :: Maybe (Double, Int) -> Maybe (Double, Int) -> Maybe (Double, Int)
nearestIntersection Nothing Nothing = Nothing
nearestIntersection Nothing x@(Just (_, _)) = x
nearestIntersection y@(Just (_, _)) Nothing = y
nearestIntersection x@(Just (d1, _)) y@(Just (d2, _)) | d1 < d2 = x 
                                                      | otherwise = y

-- Intersect with a ray
intersect :: Ray -> Int -> Int -> SparseOctree -> Maybe (Double, Int)
intersect _ _ _ SparseOctreeDummy = Nothing
intersect ray depth maxDepth (SparseOctreeNode box children)
  | depth >= maxDepth = case intersectRayAABB box ray of Nothing -> Nothing
                                                         Just dist -> Just (dist, 0)
  | otherwise = case intersectRayAABB box ray of Nothing -> Nothing
                                                 Just _ -> foldr1 nearestIntersection (map (intersect ray (depth + 1) maxDepth) children) -- TODO : change ray according to closest intersection?

intersect ray _ _ (SparseOctreeLeaf box _) = case intersectRayAABB box ray of Nothing -> Nothing
                                                                              Just dist -> Just (dist, 0)

boundingRadius :: SparseOctree -> Double
boundingRadius SparseOctreeDummy = 0
boundingRadius (SparseOctreeNode box _) = boundingBoxRadius box
boundingRadius (SparseOctreeLeaf box _) = boundingBoxRadius box

boundingBox :: SparseOctree -> AABB
boundingBox SparseOctreeDummy = error "Invalid SVO"
boundingBox (SparseOctreeNode box _) = box
boundingBox (SparseOctreeLeaf box _) = box

-- Traverse the whole tree and make a list of bounding boxes for each leaf
enumerateLeafBoxes :: SparseOctree -> [AABB]
enumerateLeafBoxes = enumerateLeafBoxes' []
  where
    enumerateLeafBoxes' acc SparseOctreeDummy = acc
    enumerateLeafBoxes' acc (SparseOctreeNode _ children) = acc ++ concatMap (enumerateLeafBoxes' []) children
    enumerateLeafBoxes' acc (SparseOctreeLeaf box _) = box : acc
