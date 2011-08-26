-- Camera module

module Camera where

import Vector
import Matrix

data Camera = Camera { worldToCamera :: Matrix, fieldOfView :: !Double, position :: Vector } deriving (Show)

lookAt :: Position -> Position -> Direction -> Double -> Camera
lookAt pos target up fov = 
    Camera matrix fov pos
    where
      forward = normalise $ target - pos
      right = normalise $ up `cross` forward
      up' = right `cross` forward
      matrix = buildMatrix right up' forward pos

withVectors :: Position -> Direction -> Direction -> Direction -> Double -> Camera
withVectors pos basisX basisY basisZ fov = Camera matrix fov pos
    where
      matrix = buildMatrix basisX basisY basisZ (Vector.negate pos)
