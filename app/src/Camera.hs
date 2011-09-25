-- Camera module

module Camera where

import PolymorphicNum
import Vector
import Matrix

data Camera = Camera { worldToCamera :: Matrix, fieldOfView :: !Double, position :: Vector, farClip :: !Double } deriving (Show)

lookAt :: Position -> Position -> Direction -> Double -> Double -> Camera
lookAt pos target up fov dist = 
    Camera matrix fov pos dist
    where
      forward = normalise $ target <-> pos
      right = normalise $ up `cross` forward
      up' = right `cross` forward
      matrix = buildMatrix right up' forward pos

withVectors :: Position -> Direction -> Direction -> Direction -> Double -> Double -> Camera
withVectors pos basisX basisY basisZ fov dist = Camera matrix fov pos dist
    where
      matrix = buildMatrix basisX basisY basisZ (Vector.negate pos)
