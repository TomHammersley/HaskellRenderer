-- The irradiance cache

module IrradianceCache (IrradianceCacheTree, query) where

import Vector
import Colour
import Misc
import BoundingBox
import Octree hiding (insert)
import SceneGraph

type CacheSample = (Position, Direction, Colour, Float)

data IrradianceCacheTree = IrradianceCacheDummy AABB
                         | IrradianceCacheNode AABB [IrradianceCacheTree]
                         | IrradianceCacheLeaf AABB CacheSample deriving (Show, Read, Eq)

-- Create the initial irradiance cache tree
initialiseCache :: SceneGraph -> IrradianceCacheTree
initialiseCache sceneGraph = IrradianceCacheNode (finiteBox sceneGraph) $ map IrradianceCacheDummy (generateOctreeBoxList (finiteBox sceneGraph))

-- Quantify the error if we use a given sample to shade a point
errorWeight :: (Position, Direction) -> CacheSample -> Float
errorWeight (pos', dir') (pos, dir, _, r) = 1 / (pos `distance` pos' / r + sqrt (1 + dir `dot3` dir'))

-- Divide up a node into the eight children
divideNode :: IrradianceCacheTree -> [IrradianceCacheTree]
divideNode (IrradianceCacheNode box []) = map (\x -> IrradianceCacheNode x []) $ generateOctreeBoxList box
divideNode _ = undefined

-- Insert a new sample into an octree
insert :: CacheSample -> IrradianceCacheTree -> IrradianceCacheTree
insert (pos, dir, col, r) (IrradianceCacheDummy box) = if box `contains` pos
                                                       then IrradianceCacheLeaf box (pos, dir, col, r)
                                                       else IrradianceCacheDummy box
insert cacheSample (IrradianceCacheNode box nodeChildren) = IrradianceCacheNode box $ map (insert cacheSample) nodeChildren
insert cacheSample (IrradianceCacheLeaf box cacheSample') = insert cacheSample (IrradianceCacheNode box (map (insert cacheSample') (map IrradianceCacheDummy childBoxes)))
    where
      childBoxes = generateOctreeBoxList box
                  
-- Search the tree to see if we can find a point within a given radius
findSamples :: (Position, Direction) -> IrradianceCacheTree -> [(CacheSample, Float)]
findSamples (pos, dir) (IrradianceCacheNode box nodeChildren) = if box `contains` pos
                                                                then foldr (++) [] $ map (findSamples (pos, dir)) nodeChildren
                                                                else []
findSamples (pos, dir) (IrradianceCacheLeaf _ (samplePos, sampleDir, sampleCol, sampleR))
    | pos `distanceSq` samplePos <= sampleR * sampleR = [(sample, weight)]
    | otherwise = []
    where
      weight = errorWeight (pos, dir) (samplePos, sampleDir, sampleCol, sampleR)
      sample = (pos, dir, sampleCol, sampleR)
findSamples _ (IrradianceCacheDummy _) = []

-- Sum together a list of samples and error weights
sumSamples :: [(CacheSample, Float)] -> Colour
sumSamples samples = colourSum Colour.</> weightSum
    where
      colourSum = foldr (\((_, _, col, _), weight) b -> b + col Colour.<*> weight) colBlack samples
      weightSum = foldr (\((_, _, _, _), weight) b -> b + weight) 0 samples

-- Query the irradiance given a point
query :: IrradianceCacheTree -> (Position, TangentSpace) -> ((Position, TangentSpace) -> (Colour, Float)) -> (Colour, IrradianceCacheTree)
query irrCache posTanSpace lookup = case findSamples (fst posTanSpace, (thr . snd) posTanSpace) irrCache of
                                      [] -> (colour, insert sample irrCache)
                                            where
                                              sample = (fst posTanSpace, (thr . snd) posTanSpace, colour, r)
                                              (colour, r) = lookup posTanSpace
                                      x : xs -> (sumSamples (x:xs), irrCache)
