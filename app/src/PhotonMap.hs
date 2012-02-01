-- Photon mapping

module PhotonMap(buildPhotonMap, PhotonMap(photonList), irradiance, PhotonMapContext(PhotonMapContext)) where

import PolymorphicNum
import {-# SOURCE #-} Light hiding (position)
import Vector
import Distribution
import Material
import Colour
import SceneGraph
import RayTrace
import Ray hiding (direction)
import Control.Monad.State
import BoundingBox
import KDTree
import Debug.Trace
import Misc
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Heap hiding (partition)
import System.Random
import Data.List hiding (union, insert)
import Primitive
import RussianRoulette

data PhotonMapContext = PhotonMapContext {
      photonGatherDistance :: Double,
      maxGatherPhotons :: Int,
      coneFilterK :: Double,
      directVisualisation :: Bool }

data Photon = Photon { power :: {-# UNPACK #-} !Colour, posDir :: {-# UNPACK #-} !(Position, Direction) } deriving (Show, Eq, Ord)

data PhotonMapTree = PhotonMapNode {-# UNPACK #-} !Int {-# UNPACK #-} !Double PhotonMapTree PhotonMapTree
                   | PhotonMapLeaf {-# UNPACK #-} !Photon deriving (Show, Eq)

data PhotonMap = PhotonMap { photonList :: [Photon],
                             photonMapTree :: PhotonMapTree } deriving(Show, Eq)

instance NFData Photon where
    rnf (Photon power' posDir') = rnf power' `seq` rnf posDir'

-- TODO - Sort this out!
mtToRefactor :: StdGen
mtToRefactor = mkStdGen 12345

-- Generate a list of photon position and direction tuples to emit
-- I zip up each pos,dir tuple with a random number generator to give each photon a different sequence of random values
-- Helps parallelisation...
-- TODO Eliminate magic number seeds from here
emitPhotons :: Light -> Int -> [(Position, Direction, StdGen, Colour)]
emitPhotons (PointLight (CommonLightData lightPower True) pos _) numPhotons = zipWith (\dir num -> (pos, dir, mkStdGen num, flux)) (fst $ generatePointsOnSphere numPhotons 1 mtToRefactor) [1..numPhotons]
    where
      flux = lightPower <*> ((1.0 / fromIntegral numPhotons) :: Double)
emitPhotons (QuadLight (CommonLightData lightPower True) corner _ du dv) numPhotons = zipWith3 (\pos dir num -> (pos, transformDir dir tanSpace, mkStdGen num, flux)) randomPoints randomDirs [1..numPhotons]
    where
      randomPoints = fst $ generatePointsOnQuad corner du dv numPhotons mtToRefactor
      randomDirs = fst $ generatePointsOnHemisphere numPhotons 1 mtToRefactor
      area =  Vector.magnitude (du `cross` dv)
      flux = lightPower <*> (area / fromIntegral numPhotons)
      tanSpace = (normalise du, normalise dv, normalise (du `cross` dv))
emitPhotons _ _ = []

-- Decide what to do with a photon
choosePhotonFate :: (RandomGen g) => (Double, Double) -> State g RussianRouletteChoice
choosePhotonFate (diffuseP, specularP) = do
  p <- randDouble
  let result | p < diffuseP = DiffuseReflect
             | p < (diffuseP + specularP) = SpecularReflect
             | otherwise = Absorb
  return $! result

-- Compute new power for a photon
computeNewPhotonPower :: RussianRouletteChoice -> (Double, Double) -> Colour -> Material -> Colour
computeNewPhotonPower fate (diffuseP, specularP) photonPower mat = case fate of
                                                                     DiffuseReflect -> photonPower <*> diffuse mat </> diffuseP
                                                                     SpecularReflect -> photonPower <*> specular mat </> specularP
                                                                     Absorb -> colBlack

-- Find a diffuse reflection direction in the hemisphere of the normal
-- Realistic Image Synthesis Using Photon Mapping - Eq 2.24
diffuseReflectionDirection :: (RandomGen g) => g -> TangentSpace -> (Direction, g)
diffuseReflectionDirection stdGen tanSpace = (transformDir dir tanSpace, stdGen')
    where
      (uv, stdGen') = runState randomUV stdGen
      dir = uvToHemisphere 1 0 uv

-- Main working photon tracing function
-- Realistic Image Synthesis Using Photon Mapping p60
tracePhoton :: (RandomGen g) => [Photon] -> Photon -> SceneGraph -> g -> (Int, Int) -> [Photon]
tracePhoton currentPhotons (Photon photonPower photonPosDir) sceneGraph rndState (bounce, maxBounces) = 
  -- See if the photon intersects a surfaces
  case findNearestIntersection sceneGraph ray of
    Nothing -> currentPhotons
    Just (obj, t, tanSpace) -> case photonFate of
      -- Diffuse reflection. Here, we store the photon that got reflected, and trace a new photon - but only if it's bright enough to be worthwhile
      DiffuseReflect -> if Colour.magnitude newPhotonPower > brightnessEpsilon && (bounce + 1) <= maxBounces
                        then tracePhoton (storedPhoton : currentPhotons) reflectedPhoton sceneGraph rndState'' (bounce + 1, maxBounces)
                        else storedPhoton : currentPhotons
        where
          reflectedPhoton = Photon newPhotonPower (surfacePos, reflectedDir)
          (reflectedDir, rndState'') = diffuseReflectionDirection rndState' tanSpace
            
      -- Specular reflection. Here, we reflect the photon in the fashion that the surface would reflect towards the viewer and
      -- aim to absorb it somewhere else in the photon map
      SpecularReflect -> if Colour.magnitude newPhotonPower > brightnessEpsilon && (bounce + 1) <= maxBounces
                         then tracePhoton currentPhotons reflectedPhoton sceneGraph rndState' (bounce + 1, maxBounces)
                         else currentPhotons
        where
          reflectedPhoton = Photon newPhotonPower (surfacePos, reflectedDir)
          reflectedDir = Vector.negate (snd photonPosDir) `reflect` normal

      -- Absorb. The photon simply gets absorbed into the map
      Absorb -> storedPhoton : currentPhotons
      where
        (photonFate, rndState') = runState (choosePhotonFate coefficients) rndState
        coefficients = russianRouletteCoefficients (material obj)
        newPhotonPower = computeNewPhotonPower photonFate coefficients photonPower (material obj)
        normal = thr tanSpace
        hitPosition = pointAlongRay ray t
        surfacePos = hitPosition <+> normal <*> surfaceEpsilon
        brightnessEpsilon = 0.1
        storedPhoton = Photon photonPower (surfacePos, snd photonPosDir)
  where
    ray = rayWithPosDir photonPosDir 10000

-- Build a list of photons for a light source
tracePhotonsForLight :: Int -> SceneGraph -> Light -> [Photon]
tracePhotonsForLight numPhotons sceneGraph light = concat (map (\(pos, dir, rndState, flux) -> tracePhoton [] (Photon flux (pos, dir)) sceneGraph rndState (0, maxBounces)) posDirGens `using` parListChunk photonsPerChunk rdeepseq)
    where
      posDirGens = emitPhotons light numPhotons -- Positions, directions, random number generators
      maxBounces = 500
      photonsPerChunk = 256

-- High-level function to build a photon map
buildPhotonMap :: SceneGraph -> [Light] -> Int -> (PhotonMap, [Light])
buildPhotonMap sceneGraph lights numPhotonsPerLight = photons `seq` kdTree `seq` (PhotonMap photons kdTree, lightsNotForPhotonMap)
    where
      (lightsForPhotonMap, lightsNotForPhotonMap) = partition (addToPhotonMap . common) lights
      photons = concatMap (tracePhotonsForLight numPhotonsPerLight sceneGraph) lightsForPhotonMap
      kdTree = buildKDTree photons

-- Make a bounding box of a list of photons
photonsBoundingBox :: [Photon] -> AABB
photonsBoundingBox = foldl' (\box photon -> boundingBoxEnlarge (fst . posDir $ photon) box) initialInvalidBox

-- Construct a balanced kd tree of photons
-- Realistic Image Synthesis Using Photon Mapping p72
buildKDTree :: [Photon] -> PhotonMapTree
buildKDTree (x:[]) = PhotonMapLeaf x
buildKDTree [] = error "buildKDTree [] should never get called"
buildKDTree photons = let (boxMin, boxMax) = photonsBoundingBox photons
                          axis = largestAxis (boxMax <-> boxMin)
                          numPhotons = fromIntegral (length photons) :: Double
                          photonsMedian = foldl' (\box photon -> box <+> (fst . posDir $ photon)) zeroVector photons </> numPhotons
                          value = component photonsMedian axis
                          photonsGT = Prelude.filter (\p -> component ((fst . posDir) p) axis > value) photons
                          photonsLE = Prelude.filter (\p -> component ((fst . posDir) p) axis <= value) photons
                      in if length photonsGT > 0 && length photonsLE > 0
                         then let gtTree = buildKDTree photonsGT
                                  leTree = buildKDTree photonsLE
                              in gtTree `seq` leTree `seq` PhotonMapNode axis value gtTree leTree
                         else let (photons0', photons1') = trace "Using degenerate case" $ degenerateSplitList photons in PhotonMapNode axis value (buildKDTree photons0') (buildKDTree photons1')

-- Use a max heap to make it easy to eliminate distant photons
data GatheredPhoton = GatheredPhoton Double Photon deriving (Show)
type PhotonHeap = MaxHeap GatheredPhoton

instance Ord GatheredPhoton where
    compare (GatheredPhoton dist1 _) (GatheredPhoton dist2 _) = dist1 `compare` dist2

instance Eq GatheredPhoton where
    (GatheredPhoton dist1 _) == (GatheredPhoton dist2 _) = dist1 == dist2

instance NFData GatheredPhoton where
    rnf (GatheredPhoton dist photon) = rnf dist `seq` rnf photon

-- Return the minimum squared search radius from that specified, versus the furthest photon in the heap
-- We don't want to locate any photons further away than our current furthest - we're looking for the closest ones, after all
minimalSearchRadius :: Double -> PhotonHeap -> Double
minimalSearchRadius rSq photonHeap = case viewHead photonHeap of
                                       Nothing -> rSq
                                       Just (GatheredPhoton dSq _) -> Prelude.min rSq dSq

-- Gather photons for irradiance computations
-- Algorithm adapted from Realistic Image Synthesis Using Photon Mapping p73
gatherPhotons :: PhotonMapTree -> Position -> Double -> PhotonHeap -> Int -> PhotonHeap
gatherPhotons (PhotonMapNode axis value gtChild leChild) pos rSq photonHeap maxPhotons
    -- In this case, the split plane bisects the search sphere - search both halves of tree
    | (value - posComponent) ** 2 <= rSq = let heap1 = gatherPhotons gtChild pos rSq' photonHeap maxPhotons
                                               rSq'' = minimalSearchRadius rSq' heap1
                                               heap2 = gatherPhotons leChild pos rSq'' photonHeap maxPhotons
                                               newHeap = union heap1 heap2
                                           in heap1 `seq` heap2 `seq` newHeap `seq` Data.Heap.drop (size newHeap - maxPhotons) newHeap

    -- One side of the tree...
    | posComponent > value = gatherPhotons gtChild pos rSq' photonHeap maxPhotons

    -- ... or the other
    | posComponent <= value = gatherPhotons leChild pos rSq' photonHeap maxPhotons

    -- Prolapse
    | otherwise = error "gatherPhotons: unexplained/unexpected case here"
    where
      posComponent = component pos axis
      rSq' = minimalSearchRadius rSq photonHeap -- Refine search radius as we go down tree to search no further than closest allowed photon
gatherPhotons (PhotonMapLeaf p) pos rSq photonHeap maxPhotons
    | distSq < rSq = let newHeap = insert (GatheredPhoton distSq p) photonHeap
                     in Data.Heap.drop (size newHeap - maxPhotons) newHeap -- Discard any excess photons - we get rid of the furthest ones
    | otherwise = photonHeap
    where distSq = pos `distanceSq` (fst . posDir) p

-- Return the contribution of a given photon, including a simple cos term to emulate BRDF plus the cone filter
-- Cone filter is from Realistic Image Synthesis Using Photon Mapping p81
photonContribution :: Double -> SurfaceLocation -> Photon -> Colour
photonContribution kr (pos, (_, _, normal)) photon = power photon <*> ((Vector.negate normal `sdot3` (snd . posDir) photon) * weight)
    where
      weight = 1 - (pos `distance` (fst . posDir) photon) / (kr + 0.000000001) -- Add on an epsilon to prevent div0 in cone filter

-- Find the overall contribution of a list of photons
-- Radiance estimate algorithm from Realistic Image Synthesis Using Photon Mapping p81
sumPhotonContribution :: Double -> Double -> SurfaceLocation -> [Photon] -> Colour
sumPhotonContribution r k posTanSpace photons = foldl' (\y x -> y <+> photonContribution (k * r) posTanSpace x) colBlack photons <*> (1.0 / ((1.0 - 2.0 / (3.0 * k)) * pi * r * r))

-- Look up the resulting irradiance from the photon map at a given point
-- Realistic Image Synthesis Using Photon Mapping, e7.6
irradiance :: PhotonMap -> PhotonMapContext -> Material -> SurfaceLocation -> (Colour, Double)
irradiance photonMap photonMapContext mat posTanSpace = (sumPhotonContribution r k posTanSpace gatheredPhotons <*> diffuse mat, harmonicMean $ map (\(GatheredPhoton dist _) -> sqrt dist) nearestPhotons)
    where
      r = photonGatherDistance photonMapContext
      maxPhotons
          | directVisualisation photonMapContext = 1
          | otherwise = maxGatherPhotons photonMapContext
      k = coneFilterK photonMapContext
      photonHeap = gatherPhotons (photonMapTree photonMap) (fst posTanSpace) (r * r) Data.Heap.empty maxPhotons
      nearestPhotons = Data.Heap.take maxPhotons photonHeap
      gatheredPhotons = map (\(GatheredPhoton _ photon) -> photon) nearestPhotons
