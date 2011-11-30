-- The sparse voxel octree data structure

module SparseVoxelOctree where

import OctTree

data SparseOctTree a = SparseOctTreeDummy !AABB
                     | SparseOctTreeNode !AABB [SparseOctTree a]
                     | SparseOctTreeLeaf !AABB !(Vector, a) deriving (Eq)

instance Show a => Show (SparseOctTree a) where
    show = display 0

tabs :: String
tabs = '\t' : tabs

display :: (Show a) => Int -> SparseOctTree a -> String
display level (SparseOctTreeDummy box) = take level tabs ++ "[Dummy] box=" ++ show box ++ "\n"
display level (SparseOctTreeNode box children) = take level tabs ++ "[Node] box=" ++ show box ++ "\n" ++ concatMap (display (level + 1)) children ++ "\n"
display level (SparseOctTreeLeaf box (pos, value)) = take level tabs ++ "[Leaf] box=" ++ show box ++ " pos=" ++ show pos ++ " value=" ++ show value ++ "\n"

create :: AABB -> SparseOctTree a
create box = SparseOctTreeNode box $ map SparseOctTreeDummy (generateOctreeBoxList box)

-- Insert into an octree
insert :: Vector -> a -> SparseOctTree a -> SparseOctTree a
insert pos a oct = fst $ insert' pos oct (Just a)

insert' :: Vector -> SparseOctTree a -> Maybe a -> (SparseOctTree a, Maybe a)
insert' pos oct@(SparseOctTreeDummy box) state = case state of
                                             -- If we have been passed some state then attempt to consume it
                                             Just value -> if box `contains` pos
                                                           then (SparseOctTreeLeaf box (pos, value), Nothing)
                                                           else (oct, state)
                                             _ -> (oct, state)

insert' pos oct@(SparseOctTreeNode box nodeChildren) state = if box `contains` pos
                                                       then let (nodeChildren', state') = mapS (insert' pos) nodeChildren state 
                                                            in (SparseOctTreeNode box nodeChildren', state')
                                                       else (oct, state)

insert' pos oct@(SparseOctTreeLeaf box (pos', a')) state = if box `contains` pos 
                                                     then 
                                                         -- First up, we turn this leaf into a node with 8 children
                                                         -- Discard result of mapS - we assume it returns Nothing
                                                         -- Then, re-insert the original value into our nascent octree
                                                         let (!newChildren, _) = mapS (insert' pos) (map SparseOctTreeDummy (generateOctreeBoxList box)) state
                                                             (!octTree', !state') = insert' pos' (SparseOctTreeNode box newChildren) (Just a')
                                                         in (octTree', state')
                                                     else (oct, state)

-- Gather data within a sphere from an octree
gather :: Position -> Double -> SparseOctTree a -> [(a, Double)]
gather pos r (SparseOctTreeNode box nodeChildren) = if overlapsSphere box pos r
                                              then concatMap (gather pos r) nodeChildren
                                              else []
gather pos r (SparseOctTreeLeaf _ (pos', a))
    | dSq <= r * r = [(a, dSq)]
    | otherwise = []
    where dSq = pos `distanceSq` pos'
gather _ _ (SparseOctTreeDummy _) = []

-- Build a sparse voxel octree for a data set
build :: (Position -> Bool) -> AABB - Int -> SparseVoxelOctTree
build func box maxDepth = build' func box 0 maxDepth

build' :: (Position -> Bool) -> AABB - Int -> SparseVoxelOctTree
build' func box depth maxDepth
  | depth == maxDepth = error "Should not have been called to this depth"
  | otherwise = SparseOctTreeDummy box
