-- The sparse voxel octree data structure

module SparseVoxelOctree(build, 
                         SparseOctree, 
                         closestIntersect, 
                         boundingRadius, 
                         boundingBox,
                         enumerateLeafBoxes) where

import Octree
import BoundingBox
import Ray
import Vector

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
build :: (AABB -> Double) -> AABB -> Int -> Double -> SparseOctree
build = build' 0
  where
    build' depth func box maxDepth minDimension
      | func box <= 0 = SparseOctreeDummy -- Really this is a soft error. But, the user might specify an invalid input...
      | depth == maxDepth || boundingBoxRadius box <= minDimension = SparseOctreeLeaf box (func box) -- TODO Or size of box < threshold
      | otherwise = SparseOctreeNode box (concatMap buildNonEmptyNodes subBoxList)
      where
        subBoxList = splitBoxIntoOctreeChildren box
        buildNonEmptyNodes childBox 
          | func childBox > 0 = [build' (depth + 1) func childBox maxDepth minDimension]
          | otherwise = []
                                         
-- Find the closest Maybe intersection
nearestIntersection :: Maybe (Double, TangentSpace) -> Maybe (Double, TangentSpace) -> Maybe (Double, TangentSpace)
nearestIntersection Nothing Nothing = Nothing
nearestIntersection Nothing x@(Just _) = x
nearestIntersection y@(Just _) Nothing = y
nearestIntersection x@(Just (d1, _)) y@(Just (d2, _)) | d1 < d2 = x 
                                                      | otherwise = y

-- Intersect with a ray
closestIntersect :: Ray -> Int -> Int -> SparseOctree -> Maybe (Double, TangentSpace)
closestIntersect _ _ _ SparseOctreeDummy = Nothing
closestIntersect ray depth maxDepth (SparseOctreeNode box children) =
  case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                          Just (dist, dist2) -> if depth >= maxDepth then Just (dist, boundingBoxTangentSpace box (pointAlongRay ray dist))
                                                                else foldr1 nearestIntersection (map (closestIntersect (shortenRay ray dist2) (depth + 1) maxDepth) children)
closestIntersect ray _ _ (SparseOctreeLeaf box _) = case boundingBoxIntersectRay box ray of Nothing -> Nothing
                                                                                            Just (dist, _) -> Just (dist, boundingBoxTangentSpace box (pointAlongRay ray dist))

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
