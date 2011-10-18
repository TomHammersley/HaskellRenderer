-- Module for generating sample patterns for distributed ray tracing

module Distribution (generatePointsOnSphere, 
                     generatePointsOnQuad, 
                     generatePointsOnHemisphere,
                     generatePointOnHemisphere,
                     generateRandomUVs,
                     generateDirectionOnHemisphere,
                     generateDirectionsOnHemisphere,
                     generateDirectionsOnSphere,
                     generateDirectionsOnHemisphereUnstratified,
                     randomUV) where

import PolymorphicNum
import Vector
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Misc

type GeneratorState = State PureMT

-- Generate a pair of random normalised floats
randomUV :: GeneratorState (Double, Double)
randomUV = do generator <- get
              let (u, generator') = randomDouble generator
              let (v, generator'') = randomDouble generator'
              put generator''
              return (saturate u, saturate v)

-- Generate a list of N random UVs
generateRandomUVs :: Int -> GeneratorState [(Double, Double)]
generateRandomUVs n = replicateM n randomUV

uvToSphere :: Double -> (Double, Double) -> Position
uvToSphere r (u, v) = Vector (r * x) (r * y) (r * z) 1
    where
      z = 2 * u - 1
      t = 2 * pi * v
      w = sqrt (1 - z * z)
      x = w * cos t
      y = w * sin t

-- Proportional to cosine-weighted solid angle
uvToHemisphere :: Double -> Double -> (Double, Double) -> Position
{-# SPECIALIZE INLINE uvToHemisphere :: Double -> Double -> (Double, Double) -> Position #-}
uvToHemisphere r w (u, v) = Vector (r * x) (r * y) (r * z) w
    where
      k = sqrt u
      theta = 2.0 * pi * v
      x = k * cos theta
      y = k * sin theta
      z = sqrt (1.0 - u)
--      z = sqrt (0 `Prelude.max` (1 - u))

-- Generate a list of random points on a sphere
generatePointsOnSphere :: Int -> Double -> PureMT -> ([Position], PureMT)
generatePointsOnSphere numPoints r mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | otherwise = (map (uvToSphere r) randomUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt

-- Generate a list of random points on a hemisphere (z > 0)
generatePointsOnHemisphere :: Int -> Double -> PureMT -> ([Position], PureMT)
generatePointsOnHemisphere numPoints r mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | otherwise = (map (uvToHemisphere r 1) randomUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt

generatePointsOnQuad :: Position -> Direction -> Direction -> Int -> PureMT -> ([Position], PureMT)
generatePointsOnQuad pos deltaU deltaV numPoints mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | otherwise = (map (\(u, v) -> pos <+> deltaU <*> u <+> deltaV <*> v) randomUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt

-- Generate a single random point on a hemisphere
generatePointOnHemisphere :: PureMT -> Double -> (Position, PureMT)
generatePointOnHemisphere rndGen r  = (uvToHemisphere r 1 uv, rndGen')
    where
      (uv, rndGen') = runState randomUV rndGen

-- Stratify over an 8x8 grid
stratify :: (Double, Double) -> Int -> (Double, Double)
stratify (u, v) index = ((col + u) * recipGridX, (row + v) * recipGridY)
    where
      gridX = 8
      gridY = 8
      recipGridX = (1.0 :: Double) / gridX
      recipGridY = (1.0 :: Double) / gridY
      wrappedIndex = index `mod` floor (gridX * gridY)
      row = fromIntegral (wrappedIndex `div` floor gridX)
      col = fromIntegral (wrappedIndex `mod` floor gridX)

generateDirectionsOnHemisphere :: Int -> Double -> PureMT -> ([Direction], PureMT)
generateDirectionsOnHemisphere numPoints r mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | numPoints `mod` 64 /= 0 = error "Error, must specify point count in multiples of 64 (8x8 grid stratification)"
    | otherwise = (map (uvToHemisphere r 0) stratifiedUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt
      stratifiedUVs = zipWith stratify randomUVs [0..]

generateDirectionsOnSphere :: Int -> Double -> PureMT -> ([Direction], PureMT)
generateDirectionsOnSphere numPoints r mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | otherwise = (map (setWTo0 . uvToSphere r) randomUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt

generateDirectionOnHemisphere :: PureMT -> Double -> (Direction, PureMT)
generateDirectionOnHemisphere mt r = (uvToHemisphere r 0 uv, mt')
    where
      (uv, mt') = runState randomUV mt

generateDirectionsOnHemisphereUnstratified :: Int -> Double -> PureMT -> ([Direction], PureMT)
generateDirectionsOnHemisphereUnstratified numPoints r mt
    | numPoints <= 1 = ([Vector 0 0 0 1], mt)
    | otherwise = (map (uvToHemisphere r 0) randomUVs, mt')
    where
      (randomUVs, mt') = runState (generateRandomUVs numPoints) mt
