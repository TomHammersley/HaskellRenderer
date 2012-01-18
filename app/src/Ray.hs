-- Module for handling rays in a raytracer

module Ray where

import PolymorphicNum
import Vector

-- For now, we're sticking to Doubles
data Ray = Ray { origin :: {-# UNPACK #-} !Position, direction :: {-# UNPACK #-} !Direction, invDirection :: {-# UNPACK #-} !Direction, rayLength :: {-# UNPACK #-} !Double } deriving (Show)

-- Make a ray given the start and end position
rayWithPoints :: Position -> Position -> Ray
rayWithPoints start end = Ray start dir (recipPerElem dir) d
  where
    d = end `distance` start
    dir = (end <-> start) </> d

rayWithDirection :: Position -> Direction -> Double -> Ray
rayWithDirection p d = Ray p d (recipPerElem d)

rayWithPosDir :: (Position, Direction) -> Double -> Ray
rayWithPosDir (start, dir) = Ray start dir (recipPerElem dir) -- ray length done via eta reduction

-- Given a ray and a distance, produce the point along the ray
pointAlongRay :: Ray -> Double -> Position
pointAlongRay (Ray org dir _ _) dist = setWTo1 (madd org dir dist)

-- Given some intercept, work out if it is valid, for this ray
validIntercept :: Ray -> Double -> Bool
validIntercept (Ray _ _ _ rayLen) t = t >= 0 && t <= rayLen

-- Make a shorter version of the same ray
shortenRay :: Ray -> Double -> Ray
shortenRay (Ray org dir invDir _) = Ray org dir invDir
