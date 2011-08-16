-- The irradiance cache

module IrradianceCache (IrradianceCache, query, initialiseCache) where

import Vector
import Colour
import Misc
import BoundingBox
import Octree
import SceneGraph
import Debug.Trace

data CacheSample = CacheSample (Direction, Colour, Float)

type IrradianceCache = OctTree CacheSample

-- Pretty printer for cache samples
instance Show CacheSample where
    show (CacheSample (dir, col, r)) = "\tDirection: " ++ show dir ++ "\n\tColour: " ++ show col ++ "\n\tRadius: " ++ show r ++ "\n"

-- Create the initial irradiance cache tree
initialiseCache :: SceneGraph -> IrradianceCache
initialiseCache sceneGraph = OctTreeNode (finiteBox sceneGraph) $ map OctTreeDummy (generateOctreeBoxList (finiteBox sceneGraph))

-- Quantify the error if we use a given sample to shade a point
errorWeight :: (Position, Direction) -> (Position, CacheSample) -> Float
errorWeight (pos', dir') (pos, CacheSample (dir, _, r)) = 1 / (pos `distance` pos' / r + sqrt (1 + dir `dot3` dir'))

-- Search the tree to see if we can find a point within a given radius
findSamples :: (Position, Direction) -> IrradianceCache -> [(Vector, CacheSample, Float)]
findSamples (pos, dir) (OctTreeNode box nodeChildren) = if box `contains` pos
                                                        then concatMap (findSamples (pos, dir)) nodeChildren --  foldr (++) [] $ map (findSamples (pos, dir)) nodeChildren
                                                        else []
findSamples (pos, dir) (OctTreeLeaf _ (samplePos, sample))
    | pos `distanceSq` samplePos <= sampleR * sampleR = [(samplePos, sample, weight)]
    | otherwise = []
    where
      weight = errorWeight (pos, dir) (samplePos, sample)
      (CacheSample (_, _, sampleR)) = sample
findSamples _ (OctTreeDummy _) = []

-- Sum together a list of samples and error weights
sumSamples :: [(Vector, CacheSample, Float)] -> Colour
sumSamples samples = colourSum Colour.</> weightSum
    where
      colourSum = foldr (\(_, CacheSample (_, col, _), weight) b -> b + col Colour.<*> weight) colBlack samples
      weightSum = foldr (\(_, CacheSample (_, _, _), weight) b -> b + weight) 0 samples

-- Query the irradiance given a point
-- Supplied function supplies the irradiance colour at a surface location along with the radius it is valid for
query :: IrradianceCache -> (Position, TangentSpace) -> ((Position, TangentSpace) -> (Colour, Float)) -> (Colour, IrradianceCache)
query irrCache posTanSpace f = case findSamples (fst posTanSpace, (thr . snd) posTanSpace) irrCache of
                                 [] -> trace ("Adding new sample to cache:\nPosition: " ++ show (fst posTanSpace) ++ "\n" ++ show sample) $ (colour, insert (fst posTanSpace) sample irrCache)
                                     where
                                       sample = CacheSample ((tsNormal . snd) posTanSpace, colour, r)
                                       (colour, r) = f posTanSpace
                                 x : xs -> trace "Using existing cache samples" $ (sumSamples (x:xs), irrCache)
