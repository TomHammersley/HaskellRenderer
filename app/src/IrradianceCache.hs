-- The irradiance cache

module IrradianceCache (IrradianceCacheTree, query) where

import Vector
import Colour
import BoundingBox
import Misc

type CacheSample = (Position, Direction, Colour, Float)

data IrradianceCacheTree = IrradianceCacheNode AABB [IrradianceCacheTree]
                         | IrradianceCacheLeaf CacheSample deriving (Show, Read, Eq)

-- Quantify the error if we use a given sample to shade a point
errorWeight :: (Position, Direction) -> CacheSample -> Float
errorWeight (pos', dir') (pos, dir, _, r) = 1 / (pos `distance` pos' / r + sqrt (1 - dir `dot3` dir'))

-- Insert a new sample into an octree
insert :: CacheSample -> IrradianceCacheTree -> IrradianceCacheTree
insert = undefined

-- Internal function to reduce down intermediate candidates to a single one
reduce :: (Maybe CacheSample, Float) -> [(Maybe CacheSample, Float)] -> (Maybe CacheSample, Float)
reduce best (x:xs) = case fst best of
                       Nothing -> reduce x xs
                       Just _ -> if snd x > snd best
                                 then reduce x xs
                                 else reduce best xs
reduce x [] = x

-- Search the tree to see if we can find a point within a given radius
findSample :: (Position, Direction) -> (Maybe CacheSample, Float) -> IrradianceCacheTree -> (Maybe CacheSample, Float)
findSample (pos, dir) currentBest (IrradianceCacheNode box children) = if contains box pos 
                                                                       then reduce (Nothing, 0) $ map (findSample (pos, dir) currentBest) children -- Needs a reduction
                                                                       else currentBest
findSample (pos, dir) currentBest (IrradianceCacheLeaf (samplePos, sampleDir, sampleCol, sampleR))
    | pos `distanceSq` samplePos <= sampleR * sampleR = case currentBest of
                                                          -- We currently don't have any best sample. By default, this is it
                                                          (Nothing, _) -> (Just (samplePos, sampleDir, sampleCol, sampleR), snd currentBest)
                                                          -- We have a current best; is this better than it?
                                                          (Just currSample, currWeight) -> let newWeight = errorWeight (pos, dir) (samplePos, sampleDir, sampleCol, sampleR)
                                                                                           in if newWeight < currWeight
                                                                                              then (Just (samplePos, sampleDir, sampleCol, sampleR), newWeight)
                                                                                              else (Just currSample, currWeight)
    | otherwise = currentBest

-- Query the irradiance given a point
query :: IrradianceCacheTree -> (Position, TangentSpace) -> ((Position, TangentSpace) -> Colour) -> (Colour, IrradianceCacheTree)
query irrCache posTanSpace lookup = case findSample (fst posTanSpace, (thr . snd) posTanSpace) (Nothing, 1000000) irrCache of
                                      -- Cache succeeded - just return as-is
                                      (Just (_, _, col, _), _) -> (col, irrCache)
                                      (Nothing, _) -> (colour, insert sample irrCache)
                                          where
                                            sample = undefined -- Compute a new sample
                                            colour = undefined
