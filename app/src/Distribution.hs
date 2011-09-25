-- Module for generating sample patterns for distributed ray tracing
{-# LANGUAGE BangPatterns #-}

module Distribution (generatePointsOnSphere, 
                     generatePointsOnQuad, 
                     generatePointsOnHemisphere,
                     generatePointOnHemisphere,
                     generateRandomUVs,
                     randomUV) where

import PolymorphicNum
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

uvToSphere :: Double -> (Double, Double) -> Position
uvToSphere r (!u, !v) = Vector (r * x) (r * y) (r * z) 1
    where
      !z = 2 * u - 1
      !t = 2 * pi * v
      !w = sqrt (1 - z * z)
      !x = w * cos t
      !y = w * sin t

uvToHemisphere :: Double -> (Double, Double) -> Position
uvToHemisphere r (!u, !v) = Vector (r * x) (r * z) (r * y) 1
    where
      !z = 2 * u - 1
      !t = pi * v
      !w = sqrt (1 - z * z)
      !x = w * cos t
      !y = w * sin t

-- Generate a list of random points on a sphere
generatePointsOnSphere :: Int -> Double -> Int -> [Position]
generatePointsOnSphere numPoints r seed 
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map (uvToSphere r) randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))

-- Generate a list of random points on a hemisphere (z > 0)
generatePointsOnHemisphere :: Int -> Double -> Int -> [Position]
generatePointsOnHemisphere numPoints r seed
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map (uvToSphere r) randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))

generatePointsOnQuad :: Position -> Direction -> Direction -> Int -> Int -> [Position]
generatePointsOnQuad pos deltaU deltaV numPoints seed 
    | numPoints <= 1 = [Vector 0 0 0 1]
    | otherwise = map (\(u, v) -> pos <+> deltaU <*> u <+> deltaV <*> v) randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (pureMT (fromIntegral seed))

-- Generate a single random point on a hemisphere
generatePointOnHemisphere :: PureMT -> Double -> (Position, PureMT)
generatePointOnHemisphere rndGen r  = (uvToHemisphere r uv, rndGen')
    where
      (uv, rndGen') = runState randomUV rndGen
