-- The irradiance cache
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module IrradianceCache (IrradianceCache, query, initialiseCache) where

import Vector
import Colour
import BoundingBox
import Octree
import SceneGraph
import Data.List

data CacheSample = CacheSample !(Direction, Colour, Double)

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
errorWeight (pos', dir') (pos, CacheSample (dir, _, r)) = 1 / ((pos `distance` pos') / r + sqrt (1 + (dir `sdot3` dir')))

-- This slightly convoluted version is written to be tail recursive. I effectively have to maintain a software stack of the
-- nodes remaining to be traversed
findSamplesTR :: (Position, Direction) -> [IrradianceCache] -> [(Vector, CacheSample, Double)] -> [(Vector, CacheSample, Double)]
findSamplesTR posDir@(!pos, _) (OctTreeNode !box nodeChildren : xs) !acc
    | box `contains` pos = findSamplesTR posDir (nodeChildren ++ xs) acc
    | otherwise = findSamplesTR posDir xs acc
findSamplesTR posDir@(!pos, _) (OctTreeLeaf _ (!samplePos, sample) : xs) !acc
    | (pos `distanceSq` samplePos) <= sampleR * sampleR && weight > minimumWeight = findSamplesTR posDir xs ((samplePos, sample, weight) : acc)
    | otherwise = findSamplesTR posDir xs acc
    where
      !weight = errorWeight posDir (samplePos, sample)
      (CacheSample (_, _, !sampleR)) = sample
      minimumWeight = 0.1
findSamplesTR posDir (OctTreeDummy _ : xs) !acc = findSamplesTR posDir xs acc
findSamplesTR _ [] !acc = acc

-- Sum together a list of samples and error weights
-- TODO Perhaps directly implemented a tail-recursive version?
sumSamples :: [(Vector, CacheSample, Double)] -> Colour
sumSamples !samples = colourSum Colour.</> weightSum
    where
      !colourSum = foldl' (\ !b (_, CacheSample (_, !col, _), !weight) -> b + col Colour.<*> weight) colBlack samples
      !weightSum = foldl' (\ !b (_, CacheSample (_, _, _), !weight) -> b + weight) 0 samples

-- Handy little debug function to easily short-circuit the irradiance cache
enableIrradianceCache :: Bool
enableIrradianceCache = True

-- Query the irradiance given a point
-- Supplied function supplies the irradiance colour at a surface location along with the radius it is valid for
query :: IrradianceCache -> SurfaceLocation -> (SurfaceLocation -> (Colour, Double)) -> (Colour, IrradianceCache)
query irrCache !posTanSpace f = if enableIrradianceCache
                                then case findSamplesTR (position, normal) [irrCache] [] of
                                       -- Insert a new cach sample
                                       [] -> let (!colour, !r) = f posTanSpace 
                                                 !sample = CacheSample (normal, colour, r)
                                             in (colour, Octree.insert (fst posTanSpace) sample irrCache)
                                       -- Re-use existing cache samples
                                       list -> (sumSamples list, irrCache)
                                else let (!colour, _) = f posTanSpace 
                                     in (colour, irrCache)
    where
      !position = fst posTanSpace
      !tanSpace = snd posTanSpace
      !normal = tsNormal tanSpace
