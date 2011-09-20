-- Module for generating sample patterns for distributed ray tracing
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Distribution (generatePointsOnSphere, generatePointsOnQuad, generatePointsOnHemisphere) where

import Vector
import System.Random.Mersenne.Pure64
import Control.Monad.State

type GeneratorState = State PureMT

-- Generate a pair of random normalised floats
randomUV :: GeneratorState (Double, Double)
randomUV = do generator <- get
              let !(u, generator') = randomDouble generator
              let !(v, generator'') = randomDouble generator'
              put generator''
              return (u, v)

-- Generate a list of N random UVs
generateRandomUVs :: Int -> GeneratorState [(Double, Double)]
generateRandomUVs n = replicateM n randomUV

-- Generate a list of random points on a sphere
generatePointsOnSphere :: Int -> Double -> Int -> [Position]
generatePointsOnSphere numPoints r seed 
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map uvToPosition randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))
      uvToPosition (!u, !v) = Vector (r * x) (r * y) (r * z) 1
          where
            !z = 2 * u - 1
            !t = 2 * pi * v
            !w = sqrt (1 - z * z)
            !x = w * cos t
            !y = w * sin t

-- Generate a list of random points on a hemisphere (z > 0)
generatePointsOnHemisphere :: Int -> Double -> Int -> [Position]
generatePointsOnHemisphere numPoints r seed
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map uvToPosition randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))
      uvToPosition (!u, !v) = Vector (r * x) (r * y) (r * z) 1
          where
            !y = 2 * u - 1
            !t = pi * v
            !w = sqrt (1 - y * y)
            !x = w * cos t
            !z = w * sin t

generatePointsOnQuad :: Position -> Direction -> Direction -> Int -> Int -> [Position]
generatePointsOnQuad pos deltaU deltaV numPoints seed 
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map uvToPosition randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))
      uvToPosition (u, v) = pos + (deltaU <*> u) + (deltaV <*> v)
