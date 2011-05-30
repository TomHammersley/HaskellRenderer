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

-- Query the irradiance given a point
query :: IrradianceCacheTree -> (Position, TangentSpace) -> ((Position, TangentSpace) -> Colour) -> Colour
query irrCache posTanSpace lookup = undefined
