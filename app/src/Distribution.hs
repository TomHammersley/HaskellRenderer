-- Module for generating sample patterns for distributed ray tracing

module Distribution (generatePointsOnSphere, 
                     generatePointsOnQuad, 
                     generatePointsOnHemisphere,
                     generatePointOnHemisphere,
                     generateRandomUVs,
                     generateDirectionsOnSphere,
                     generateStratifiedDirectionOnHemisphere,
                     generateUnstratifiedDirectionOnHemisphere,
                     generateStratifiedDirectionsOnHemisphere,
                     generateUnstratifiedDirectionsOnHemisphere,
                     randomUV,
                     stratify,
                     uvToHemisphere) where

import PolymorphicNum
import Vector
import System.Random
import Control.Monad.State
import Misc

-- Generate a pair of random normalised floats
randomUV :: (RandomGen g) => State g (Double, Double)
randomUV = do u <- randDouble
              v <- randDouble
              return (saturate u, saturate v)

-- Generate a list of N random UVs
generateRandomUVs :: (RandomGen g) => Int -> State g [(Double, Double)]
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

-- Generate a list of random points on a sphere
generatePointsOnSphere :: (RandomGen g) => Int -> Double -> g -> ([Position], g)
generatePointsOnSphere numPoints r gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | otherwise = (map (uvToSphere r) randomUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen

-- Generate a list of random points on a hemisphere (z > 0)
generatePointsOnHemisphere :: (RandomGen g) => Int -> Double -> g -> ([Position], g)
generatePointsOnHemisphere numPoints r gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | otherwise = (map (uvToHemisphere r 1) randomUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen

generatePointsOnQuad :: (RandomGen g) => Position -> Direction -> Direction -> Int -> g -> ([Position], g)
generatePointsOnQuad pos deltaU deltaV numPoints gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | otherwise = (map (\(u, v) -> pos <+> deltaU <*> u <+> deltaV <*> v) randomUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen

-- Generate a single random point on a hemisphere
generatePointOnHemisphere :: (RandomGen g) => g -> Double -> (Position, g)
generatePointOnHemisphere gen r  = (uvToHemisphere r 1 uv, gen')
    where
      (uv, gen') = runState randomUV gen

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

generateDirectionsOnSphere :: (RandomGen g) => Int -> Double -> g -> ([Direction], g)
generateDirectionsOnSphere numPoints r gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | otherwise = (map (setWTo0 . uvToSphere r) randomUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen

generateUnstratifiedDirectionOnHemisphere :: (RandomGen g) => Double -> State g Direction
generateUnstratifiedDirectionOnHemisphere r = do
  u <- randDouble
  v <- randDouble
  return (uvToHemisphere r 0 (u, v))

generateStratifiedDirectionOnHemisphere :: (RandomGen g) => g -> Double -> Int -> (Direction, g)
generateStratifiedDirectionOnHemisphere gen r index = (uvToHemisphere r 0 (stratify uv index), gen')
    where
      (uv, gen') = runState randomUV gen

generateStratifiedDirectionsOnHemisphere :: (RandomGen g) => Int -> Double -> g -> ([Direction], g)
generateStratifiedDirectionsOnHemisphere numPoints r gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | numPoints `mod` 64 /= 0 = error "Error, must specify point count in multiples of 64 (8x8 grid stratification)"
    | otherwise = (map (uvToHemisphere r 0) stratifiedUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen
      stratifiedUVs = zipWith stratify randomUVs [0..]

generateUnstratifiedDirectionsOnHemisphere :: (RandomGen g) => Int -> Double -> g -> ([Direction], g)
generateUnstratifiedDirectionsOnHemisphere numPoints r gen
    | numPoints <= 1 = ([Vector 0 0 0 1], gen)
    | otherwise = (map (uvToHemisphere r 0) randomUVs, gen')
    where
      (randomUVs, gen') = runState (generateRandomUVs numPoints) gen
