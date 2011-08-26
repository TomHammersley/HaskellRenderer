-- Module for generating sample patterns for distributed ray tracing

module Distribution (generatePointsOnSphere, generatePointsOnQuad) where

import Vector
import Random
import Control.Monad.State

type GeneratorState = State StdGen

-- Generate a pair of random normalised floats
randomUV :: GeneratorState (Double, Double)
randomUV = do generator <- get
              let !(u, newGenerator) = randomR (0.0::Double, 1.0::Double) generator
              let !(v, newGenerator') = randomR (0.0::Double, 1.0::Double) newGenerator
              put newGenerator'
              return (u, v)

-- Generate a list of N random UVs
generateRandomUVs :: Int -> GeneratorState [(Double, Double)]
generateRandomUVs n = replicateM n randomUV

-- Generate a list of random points on a unit sphere
generatePointsOnSphere :: Int -> Double -> [Position]
generatePointsOnSphere numPoints r = map uvToPosition randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (mkStdGen 12345)
      uvToPosition (u, v) = Vector (r * x) (r * y) (r * z) 1
          where
            z = 2 * u - 1
            t = 2 * pi * v
            w = sqrt (1 - z * z)
            x = w * cos t
            y = w * sin t

generatePointsOnQuad :: Position -> Direction -> Direction -> Int -> [Position]
generatePointsOnQuad pos deltaU deltaV numPoints = map uvToPosition randomUVs
    where
      randomUVs = evalState (generateRandomUVs numPoints) (mkStdGen 12345)
      uvToPosition (u, v) = pos + (deltaU <*> u) + (deltaV <*> v)
