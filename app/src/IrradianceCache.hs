-- The irradiance cache
{-# LANGUAGE BangPatterns #-}

module IrradianceCache (IrradianceCache, query, initialiseCache) where

import Vector
import Colour
import BoundingBox
import Octree
import SceneGraph
import Debug.Trace

data CacheSample = CacheSample (Direction, Colour, Float)

type IrradianceCache = OctTree CacheSample

-- Pretty printer for cache samples
instance Show CacheSample where
    show (CacheSample (dir, col, r)) = "\tDirection: " ++ show dir ++ "\n\tColour: " ++ show col ++ "\n\tRadius: " ++ show r ++ "\n"

initialiseCache :: SceneGraph -> IrradianceCache
initialiseCache sceneGraph = OctTreeNode slightlyEnlargedBox $ map OctTreeDummy (generateOctreeBoxList slightlyEnlargedBox)
    where
      -- Create the initial irradiance cache tree. This is a box a little larger than the world so that we fit any points offset along the normal etc
      slightlyEnlargedBox = growBoundingBox (finiteBox sceneGraph) 10

-- Quantify the error if we use a given sample to shade a point
errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Float
errorWeight (!pos', !dir') (!pos, CacheSample (!dir, _, !r)) = 1 / (pos `distance` pos' / r + sqrt (1 + dir `dot3` dir'))

-- Find samples that make a useful contribution
findSamples :: (Position, Direction) -> IrradianceCache -> [(Vector, CacheSample, Float)]
findSamples (!pos, !dir) (OctTreeNode box nodeChildren) = if box `contains` pos
                                                          then concatMap (findSamples (pos, dir)) nodeChildren
                                                          else {- trace ("Box " ++ show box ++ " does not contains pos " ++ show pos ++ "\n") $ -} []
findSamples (!pos, !dir) (OctTreeLeaf _ (!samplePos, sample))
    | pos `distanceSq` samplePos <= sampleR * sampleR && weight > 0 = [(samplePos, sample, weight)]
    | otherwise = []
    where
      !weight = errorWeight (pos, dir) (samplePos, sample)
      (CacheSample (_, _, !sampleR)) = sample
findSamples _ (OctTreeDummy _) = []

-- Sum together a list of samples and error weights
sumSamples :: [(Vector, CacheSample, Float)] -> Colour
sumSamples samples = colourSum Colour.</> weightSum
    where
      !colourSum = foldr (\(_, CacheSample (_, col, _), weight) b -> b + col Colour.<*> weight) colBlack samples
      !weightSum = foldr (\(_, CacheSample (_, _, _), weight) b -> b + weight) 0 samples

-- Query the irradiance given a point
-- Supplied function supplies the irradiance colour at a surface location along with the radius it is valid for
query :: IrradianceCache -> SurfaceLocation -> (SurfaceLocation -> (Colour, Float)) -> (Colour, IrradianceCache)
query irrCache posTanSpace f = case findSamples (position, normal) irrCache of
                                 [] -> {-trace ("Adding new sample to cache:\nPosition: " ++ show (fst posTanSpace) ++ "\n" ++ show sample) $-} (colour, insert (fst posTanSpace) sample irrCache)
                                     where
                                       (colour, r) = f posTanSpace
                                       sample = CacheSample (normal, colour, r)
                                 x : xs -> {-trace "Using existing cache samples" $-} (sumSamples (x:xs), irrCache)
    where
      position = fst posTanSpace
      tanSpace = snd posTanSpace
      normal = tsNormal tanSpace
                                                