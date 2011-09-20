-- The irradiance cache
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module IrradianceCache (IrradianceCache, query, initialiseCache) where

import Vector
import Colour
import BoundingBox
import Octree
import SceneGraph

-- Irradiance gradient using a central-differencing approach
data IrradianceGradient = CentralDifferenceGradient {-# UNPACK #-} !(Colour, Colour, Colour)

-- Direction of normal, colour, radius
data CacheSample = CacheSample {-# UNPACK #-} !(Normal, Colour, Double)

type IrradianceCache = OctTree CacheSample

-- Pretty printer for cache samples
instance Show CacheSample where
    show (CacheSample (dir, col, r)) = "\tDirection: " ++ show dir ++ "\n\tColour: " ++ show col ++ "\n\tRadius: " ++ show r ++ "\n"

-- This gives an initial empty cache that will later be populated
initialiseCache :: SceneGraph -> IrradianceCache
initialiseCache sceneGraph = OctTreeNode slightlyEnlargedBox $ map OctTreeDummy (generateOctreeBoxList slightlyEnlargedBox)
    where
      -- Create the initial irradiance cache tree. This is a box a little larger than the world so that we fit any points offset along the normal etc
      slightlyEnlargedBox = growBoundingBox (finiteBox sceneGraph) 10

-- Quantify the error if we use a given sample to shade a point
-- The bigger the number, the better the estimate
errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Double
{-# SPECIALIZE INLINE errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Double #-}
errorWeight (pos', dir') (pos, CacheSample (dir, _, r)) 
    | dot <= 0 = 0
    | otherwise = 1 / ((pos `distance` pos') / r + sqrt (1 + dot))
    where
      !dot = dir `dot3` dir'

-- This slightly convoluted version is written to be tail recursive. I effectively have to maintain a software stack of the
-- nodes remaining to be traversed
findSamples :: (Position, Direction) -> [IrradianceCache] -> [(Vector, CacheSample, Double)] -> [(Vector, CacheSample, Double)]
findSamples posDir@(!pos, _) (OctTreeNode !box nodeChildren : xs) !acc
    | box `contains` pos = findSamples posDir (nodeChildren ++ xs) acc
    | otherwise = findSamples posDir xs acc
findSamples posDir@(pos, _) (OctTreeLeaf _ (samplePos, sample) : xs) !acc
    | (pos `distanceSq` samplePos) <= sampleR * sampleR && weight > minimumWeight = findSamples posDir xs ((samplePos, sample, weight) : acc)
    | otherwise = findSamples posDir xs acc
    where
      !weight = errorWeight posDir (samplePos, sample)
      (CacheSample (_, _, !sampleR)) = sample
      minimumWeight = 1.5 -- The bigger this weight, the less it will reuse samples and the higher the quality
findSamples posDir (OctTreeDummy _ : xs) !acc = findSamples posDir xs acc
findSamples _ [] !acc = acc

-- Sum together a list of samples and error weights
sumSamples :: [(Vector, CacheSample, Double)] -> Colour
sumSamples !samples = colourSum Colour.</> weightSum
    where
      sumSamples' !(!colAcc, !weightAcc) ((_, CacheSample (_, !col, _), !weight):xs) = sumSamples' (colAcc + col Colour.<*> weight, weightAcc + weight) xs
      sumSamples' !(!colAcc, !weightAcc) [] = (colAcc, weightAcc)
      !(!colourSum, !weightSum) = sumSamples' (colBlack, 0) samples

-- Query the irradiance given a point
-- Supplied function supplies the irradiance colour at a surface location along with the radius it is valid for
query :: IrradianceCache -> SurfaceLocation -> (SurfaceLocation -> (Colour, Double)) -> (Colour, IrradianceCache)
query irrCache !posTanSpace f = case findSamples (position, normal) [irrCache] [] of
                                  -- Insert a new cache sample
                                  [] -> let (!colour, !r) = f posTanSpace 
                                            !sample = CacheSample (normal, colour, r)
                                        in (colour, Octree.insert (fst posTanSpace) sample irrCache)
                                  -- Re-use existing cache samples
                                  list -> (sumSamples list, irrCache)
    where
      !position = fst posTanSpace
      !tanSpace = snd posTanSpace
      !normal = tsNormal tanSpace
