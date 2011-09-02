-- The irradiance cache
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module IrradianceCache (IrradianceCache, query, initialiseCache) where

import Vector
import Colour
import BoundingBox
import Octree
import SceneGraph

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
errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Double
{-# SPECIALIZE INLINE errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Double #-}
errorWeight (!pos', !dir') (!pos, CacheSample (!dir, _, !r)) = 1 / ((pos `distance` pos') / r + sqrt (1 + (dir `dot3` dir')))

-- Find samples that make a useful contribution
-- findSamples :: (Position, Direction) -> IrradianceCache -> [(Vector, CacheSample, Double)]
-- findSamples posDir@(!pos, _) (OctTreeNode !box nodeChildren) = if box `contains` pos
--                                                                then concatMap (findSamples posDir) nodeChildren
--                                                                else {- trace ("Box " ++ show box ++ " does not contains pos " ++ show pos ++ "\n") $ -} []
-- findSamples posDir@(!pos, _) (OctTreeLeaf _ (!samplePos, sample))
--     | pos `distanceSq` samplePos <= sampleR * sampleR && weight > 0 = [(samplePos, sample, weight)]
--     | otherwise = []
--     where
--       !weight = errorWeight posDir (samplePos, sample)
--       (CacheSample (_, _, !sampleR)) = sample
-- findSamples _ (OctTreeDummy _) = []

-- This slightly convoluted version is written to be tail recursive. I effectively have to maintain a software stack of the
-- nodes remaining to be traversed
findSamplesTR :: (Position, Direction) -> [IrradianceCache] -> [(Vector, CacheSample, Double)] -> [(Vector, CacheSample, Double)]
findSamplesTR posDir@(!pos, _) ((OctTreeNode !box nodeChildren) : xs) !acc
    | box `contains` pos = findSamplesTR posDir (nodeChildren ++ xs) acc
    | otherwise = findSamplesTR posDir xs acc
findSamplesTR posDir@(!pos, _) ((OctTreeLeaf _ (!samplePos, sample)) : xs) !acc
    | (pos `distanceSq` samplePos) <= sampleR * sampleR && weight > 0 = findSamplesTR posDir xs ((samplePos, sample, weight) : acc)
    | otherwise = findSamplesTR posDir xs acc
    where
      !weight = errorWeight posDir (samplePos, sample)
      (CacheSample (_, _, !sampleR)) = sample
findSamplesTR posDir ((OctTreeDummy _) : xs) !acc = findSamplesTR posDir xs acc
findSamplesTR _ [] !acc = acc

-- Sum together a list of samples and error weights
sumSamples :: [(Vector, CacheSample, Double)] -> Colour
sumSamples !samples = colourSum Colour.</> weightSum
    where
      !colourSum = foldr (\(_, CacheSample (_, !col, _), !weight) !b -> b + col Colour.<*> weight) colBlack samples
      !weightSum = foldr (\(_, CacheSample (_, _, _), !weight) !b -> b + weight) 0 samples

-- Query the irradiance given a point
-- Supplied function supplies the irradiance colour at a surface location along with the radius it is valid for
query :: IrradianceCache -> SurfaceLocation -> (SurfaceLocation -> (Colour, Double)) -> (Colour, IrradianceCache)
query irrCache !posTanSpace f = case findSamplesTR (position, normal) [irrCache] [] of
                                  [] -> {-trace ("Adding new sample to cache:\nPosition: " ++ show (fst posTanSpace) ++ "\n" ++ show sample) $-} (colour, insert (fst posTanSpace) sample irrCache)
                                      where
                                        (!colour, !r) = f posTanSpace
                                        !sample = CacheSample (normal, colour, r)
                                  list -> {-trace "Using existing cache samples" $-} (sumSamples list, irrCache)
    where
      !position = fst posTanSpace
      !tanSpace = snd posTanSpace
      !normal = tsNormal tanSpace
