-- Photon mapping
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module PhotonMap(buildPhotonMap, PhotonMap(photonList), irradiance, PhotonMapContext(PhotonMapContext)) where

import {-# SOURCE #-} Light hiding (position)
import Vector
import Distribution
import Material
import Colour
import SceneGraph
import RayTrace
import Ray hiding (direction)
import Primitive
import Control.Monad.State
import BoundingBox
import KDTree
import Debug.Trace
import Misc
import Control.Parallel.Strategies
import Data.Heap hiding (partition)
import System.Random.Mersenne.Pure64
import Data.List (partition)

type GeneratorState = State PureMT

data PhotonMapContext = PhotonMapContext {
      photonGatherDistance :: Double,
      maxGatherPhotons :: Int,
      coneFilterK :: Double }

data Photon = Photon { power :: {-# UNPACK #-} !Colour, posDir :: {-# UNPACK #-} !(Position, Direction) } deriving (Show, Eq, Ord)

data PhotonMapTree = PhotonMapNode {-# UNPACK #-} !Int {-# UNPACK #-} !Double PhotonMapTree PhotonMapTree
                   | PhotonMapLeaf {-# UNPACK #-} !Photon deriving (Show, Eq)

data PhotonMap = PhotonMap { photonList :: [Photon],
                             photonMapTree :: PhotonMapTree } deriving(Show, Eq)

data PhotonChoice = DiffuseReflect | SpecularReflect | Absorb

-- Generate a list of photon position and direction tuples to emit
-- I zip up each pos,dir tuple with a random number generator to give each photon a different sequence of random values
-- Helps parallelisation...
emitPhotons :: Light -> Int -> [(Position, Direction, PureMT, Colour)]
emitPhotons (PointLight (CommonLightData !lightPower True) !pos _) !numPhotons = zipWith (\dir num -> (pos, dir, pureMT (fromIntegral num), flux)) (generatePointsOnSphere numPhotons 1) [1..numPhotons]
    where
      flux = lightPower Colour.<*> (1.0 / fromIntegral numPhotons)
emitPhotons (QuadLight (CommonLightData !lightPower True) !corner !du !dv) !numPhotons = zipWith3 (\pos dir num -> (pos, dir, pureMT (fromIntegral num), flux)) randomPoints randomDirs [1..numPhotons]
    where
      randomPoints = generatePointsOnQuad corner du dv numPhotons
      randomDirs = generatePointsOnSphere numPhotons 1
      !area =  Vector.magnitude (du `cross` dv)
      flux = lightPower Colour.<*> (area / fromIntegral numPhotons)
emitPhotons _ _ = []

-- Compute russian roulette coefficients
russianRouletteCoefficients :: Material -> (Double, Double)
russianRouletteCoefficients !mat = (diffuseP, specularP)
    where
      (Colour diffuseR diffuseG diffuseB _) = Material.diffuse mat
      (Colour specularR specularG specularB _) = Material.specular mat
      diffuseP = (diffuseR + diffuseG + diffuseB) / 3
      specularP = (specularR + specularG + specularB) / 3

-- Decide what to do with a photon
choosePhotonFate :: (Double, Double) -> GeneratorState PhotonChoice
choosePhotonFate !(diffuseP, specularP) = do
  generator <- get
  let (p, generator') = randomDouble generator
  let result | p < diffuseP = DiffuseReflect
             | p < (diffuseP + specularP) = SpecularReflect
             | otherwise = Absorb
  put generator'
  return result

-- Compute new power for a photon
computeNewPhotonPower :: PhotonChoice -> (Double, Double) -> Colour -> Material -> Colour
computeNewPhotonPower !fate !(diffuseP, specularP) photonPower !mat = case fate of
                                                                        DiffuseReflect -> photonPower * diffuse mat Colour.</> diffuseP
                                                                        SpecularReflect -> photonPower * specular mat Colour.</> specularP
                                                                        Absorb -> colBlack

-- Compute a new diffuse reflection in spherical co-ordinates
generateUV :: GeneratorState (Double, Double)
generateUV = do generator <- get
                let (u, generator') = randomDouble generator
                let (v, generator'') = randomDouble generator'
                put generator''
                return (u, v)

-- Find a diffuse reflection direction in the hemisphere of the normal
-- Realistic Image Synthesis Using Photon Mapping - Eq 2.24
diffuseReflectionDirection :: PureMT -> TangentSpace -> (Direction, PureMT)
diffuseReflectionDirection !stdGen !tanSpace = (transformDir dir tanSpace, stdGen')
    where
      ((u, v), stdGen') = runState generateUV stdGen
      !theta = acos (sqrt u)
      !phi = 2 * pi * v
      dir = sphericalToDirection theta phi

-- Main working photon tracing function
-- Realistic Image Synthesis Using Photon Mapping p60
tracePhoton :: [Photon] -> Photon -> SceneGraph -> PureMT -> (Int, Int) -> [Photon]
tracePhoton !currentPhotons (Photon !photonPower !photonPosDir) sceneGraph !rndState !(bounce, maxBounces) = 
    -- See if the photon intersects any surfaces
    case findNearestIntersection sceneGraph ray of
      Nothing -> currentPhotons
      Just (obj, t, subId) -> case photonFate of
                                -- Diffuse reflection. Here, we store the photon that got reflected, and trace a new photon - but only if it's bright enough to be worthwhile
                                DiffuseReflect -> if Colour.magnitude newPhotonPower > brightnessEpsilon && (bounce + 1) <= maxBounces
                                                  then tracePhoton (storedPhoton : currentPhotons) reflectedPhoton sceneGraph rndState'' (bounce + 1, maxBounces)
                                                  else storedPhoton : currentPhotons
                                    where
                                      !reflectedPhoton = Photon newPhotonPower (surfacePos, reflectedDir)
                                      !(reflectedDir, rndState'') = diffuseReflectionDirection rndState' tanSpace

                                -- Specular reflection. Here, we reflect the photon in the fashion that the surface would reflect towards the viewer and
                                -- aim to absorb it somewhere else in the photon map
                                SpecularReflect -> if Colour.magnitude newPhotonPower > brightnessEpsilon && (bounce + 1) <= maxBounces
                                                   then tracePhoton currentPhotons reflectedPhoton sceneGraph rndState' (bounce + 1, maxBounces)
                                                   else currentPhotons
                                    where
                                      !reflectedPhoton = Photon newPhotonPower (surfacePos, reflectedDir)
                                      !reflectedDir = Vector.negate (snd photonPosDir) `reflect` normal

                                -- Absorb. The photon simply gets absorbed into the map
                                Absorb -> storedPhoton : currentPhotons
          where
            !(photonFate, rndState') = runState (choosePhotonFate coefficients) rndState
            !coefficients = russianRouletteCoefficients (material obj)
            !newPhotonPower = computeNewPhotonPower photonFate coefficients photonPower (material obj)
            !tanSpace = primitiveTangentSpace (primitive obj) subId hitPosition obj
            !normal = thr tanSpace
            !hitPosition = pointAlongRay ray t
            !surfacePos = hitPosition + (normal Vector.<*> surfaceEpsilon)
            !brightnessEpsilon = 0.1
            !storedPhoton = Photon photonPower (surfacePos, snd photonPosDir)
    where
      !ray = rayWithPosDir photonPosDir 10000

-- Build a list of photons for a light source
tracePhotonsForLight :: Int -> SceneGraph -> Light -> [Photon]
tracePhotonsForLight !numPhotons sceneGraph !light = concat (map (\(pos, dir, rndState, flux) -> tracePhoton [] (Photon flux (pos, dir)) sceneGraph rndState (0, maxBounces)) posDirGens `using` parListChunk photonsPerChunk rseq)
    where
      posDirGens = emitPhotons light numPhotons -- Positions, directions, random number generators
      maxBounces = 50
      photonsPerChunk = 256

-- High-level function to build a photon map
buildPhotonMap :: SceneGraph -> [Light] -> Int -> (PhotonMap, [Light])
buildPhotonMap sceneGraph lights numPhotonsPerLight = (PhotonMap photons (buildKDTree photons), lightsNotForPhotonMap)
    where
      (lightsForPhotonMap, lightsNotForPhotonMap) = partition (addToPhotonMap . common) lights
      photons = foldr ((++) . tracePhotonsForLight numPhotonsPerLight sceneGraph) [] lightsForPhotonMap

-- Make a bounding box of a list of photons
photonsBoundingBox :: [Photon] -> AABB
photonsBoundingBox = foldr (enlargeBoundingBox .fst . posDir) initialInvalidBox

-- Construct a balanced kd tree of photons
-- Realistic Image Synthesis Using Photon Mapping p72
buildKDTree :: [Photon] -> PhotonMapTree
buildKDTree (x:[]) = PhotonMapLeaf x
buildKDTree (x:xs) = let !photons = x:xs
                         !(boxMin, boxMax) = photonsBoundingBox photons
                         !axis = largestAxis (boxMax - boxMin)
                         !numPhotons = fromIntegral (length photons)
                         !photonsMedian = foldr ((+) . fst . posDir) zeroVector photons Vector.</> numPhotons
                         !value = component photonsMedian axis
                         photonsGT = Prelude.filter (\p -> component ((fst . posDir) p) axis > value) photons
                         photonsLE = Prelude.filter (\p -> component ((fst . posDir) p) axis <= value) photons
                     in if length photonsGT > 0 && length photonsLE > 0
                        then PhotonMapNode axis value (buildKDTree photonsGT) (buildKDTree photonsLE)
                        else let (photons0', photons1') = trace "Using degenerate case" $ degenerateSplitList photons in PhotonMapNode axis value (buildKDTree photons0') (buildKDTree photons1')
buildKDTree [] = undefined

-- Use a max heap to make it easy to eliminate distant photons
data GatheredPhoton = GatheredPhoton Double Photon deriving (Show)
type PhotonHeap = MaxHeap GatheredPhoton

instance Ord GatheredPhoton where
    compare (GatheredPhoton dist1 _) (GatheredPhoton dist2 _)
        | dist1 == dist2 = EQ
        | dist1 <= dist2 = LT
        | otherwise = GT

instance Eq GatheredPhoton where
    (GatheredPhoton dist1 _) == (GatheredPhoton dist2 _) = dist1 == dist2

-- Return the minimum squared search radius from that specified, versus the furthest photon in the heap
-- We don't want to locate any photons further away than our current furthest - we're looking for the closest ones, after all
minimalSearchRadius :: Double -> PhotonHeap -> Double
minimalSearchRadius !rSq !photonHeap = case viewHead photonHeap of
                                         Nothing -> rSq
                                         Just (GatheredPhoton dSq _) -> Prelude.min rSq dSq

-- Gather photons for irradiance computations
-- Algorithm adapted from Realistic Image Synthesis Using Photon Mapping p73
gatherPhotons :: PhotonMapTree -> Position -> Double -> PhotonHeap -> Int -> PhotonHeap
gatherPhotons (PhotonMapNode !axis !value gtChild leChild) !pos !rSq !photonHeap !maxPhotons
    -- In this case, the split plane bisects the search sphere - search both halves of tree
    | (value - posComponent) ** 2 <= rSq = let !heap1 = gatherPhotons gtChild pos rSq' photonHeap maxPhotons
                                               !rSq'' = minimalSearchRadius rSq' heap1
                                               !heap2 = gatherPhotons leChild pos rSq'' photonHeap maxPhotons
                                               !newHeap = union heap1 heap2
                                           in Data.Heap.drop (size newHeap - maxPhotons) newHeap

    -- One side of the tree...
    | posComponent > value = gatherPhotons gtChild pos rSq' photonHeap maxPhotons

    -- ... or the other
    | posComponent <= value = gatherPhotons leChild pos rSq' photonHeap maxPhotons

    -- Prolapse
    | otherwise = error "gatherPhotons: unexplained/unexpected case here"
    where
      !posComponent = component pos axis
      !rSq' = minimalSearchRadius rSq photonHeap -- Refine search radius as we go down tree to search no further than closest allowed photon
gatherPhotons (PhotonMapLeaf !p) !pos !rSq !photonHeap !maxPhotons
    | distSq < rSq = let newHeap = insert (GatheredPhoton distSq p) photonHeap
                     in Data.Heap.drop (size newHeap - maxPhotons) newHeap -- Discard any excess photons - we get rid of the furthest ones
    | otherwise = photonHeap
    where !distSq = pos `distanceSq` (fst . posDir) p

-- Return the contribution of a given photon, including a simple cos term to emulate BRDF plus the cone filter
-- Cone filter is from Realistic Image Synthesis Using Photon Mapping p81
photonContribution :: Double -> SurfaceLocation -> Photon -> Colour
photonContribution !kr !(pos, (_, _, normal)) !photon = power photon Colour.<*> ((Vector.negate normal `sdot3` (snd . posDir) photon) * weight)
    where
      !weight = 1 - (pos `distance` (fst . posDir) photon) / (kr + 0.000000001) -- Add on an epsilon to prevent div0 in cone filter

-- Find the overall contribution of a list of photons
-- Radiance estimate algorithm from Realistic Image Synthesis Using Photon Mapping p81
sumPhotonContribution :: Double -> Double -> SurfaceLocation -> [Photon] -> Colour
sumPhotonContribution !r !k !posTanSpace !photons = foldr ((+) .photonContribution (k * r) posTanSpace) colBlack photons Colour.<*> (1.0 / ((1.0 - 2.0 / (3.0 * k)) * pi * r * r))

-- Look up the resulting irradiance from the photon map at a given point
-- Realistic Image Synthesis Using Photon Mapping, e7.6
irradiance :: PhotonMap -> PhotonMapContext -> Material -> SurfaceLocation -> Colour
irradiance photonMap !photonMapContext !mat !posTanSpace = sumPhotonContribution r k posTanSpace gatheredPhotons * diffuse mat
    where
      !r = photonGatherDistance photonMapContext
      !maxPhotons = maxGatherPhotons photonMapContext
      !k = coneFilterK photonMapContext
      !photonHeap = gatherPhotons (photonMapTree photonMap) (fst posTanSpace) (r * r) Data.Heap.empty maxPhotons
      !gatheredPhotons = map (\(GatheredPhoton _ photon) -> photon) (Data.Heap.take maxPhotons photonHeap)
