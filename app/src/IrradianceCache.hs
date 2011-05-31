-- The irradiance cache

module IrradianceCache (IrradianceCacheTree) where

type CacheSample = (Position, Direction, Colour, Float)

data IrradianceCacheTree = IrradianceCacheNode AABB [IrradianceCacheTree]
                         | IrradianceCacheLeaf CacheSample deriving (Show, Read, Eq)

-- Quantify the error if we use a given sample to shade a point
errorWeight :: (Position, Direction) -> CacheSample -> Float
errorWeight (pos', dir') (pos, dir, _, r) = 1 / (pos `distance` pos' / r + sqrt (1 - dir `dot3` dir'))

-- Insert a new sample into an octree
insert :: CacheSample -> IrradianceCacheTree -> IrradianceCacheTree
insert = undefined

-- Search the tree to see if we can find a point within a given radius
findSample :: (Position, Direction) -> (Maybe CacheSample, Float) -> IrradianceCacheTree -> (Maybe CacheSample, Float)
findSample (pos, dir) currentBest (IrradianceCacheNode box children) = if contains box pos 
                                                                       then map (findSample pos currentBest) children -- Needs a reduction
                                                                       else currentBest
findSample (pos, dir) currentBest (IrradianceCacheLeaf (samplePos, sampleDir, sampleCol, sampleR))
    | pos `distanceSq` samplePos <= sampleR * sampleR = case currentBest of
                                                          -- We currently don't have any best sample. By default, this is it
                                                          (Nothing, _) -> Just (samplePos, sampleDir, sampleCol, sampleR)
                                                          -- We have a current best; is this better than it?
                                                          (Just currSample, currWeight) -> let newWeight = errorWeight (pos, dir) sample
                                                                                           in if newWeight < currWeight
                                                                                              then (Just (samplePos, sampleDir, sampleCol, sampleR), newWeight)
                                                                                              else (Just currSample, currWeight)
    | otherwise = currentBest

-- Query the irradiance given a point
query :: IrradianceCacheTree -> (Position, TangentSpace) -> ((Position, TangentSpace) -> Colour) -> (Colour, IrradianceCacheTree)
query irrCache posTanSpace lookup = case findSample (fst posTanSpace) (Nothing, 1000000) irrCache of
                                      -- Cache succeeded - just return as-is
                                      Just (_, _, col, _) -> (col, irrCache)
                                      Nothing -> (sample, insert sample irrCache)
