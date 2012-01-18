-- Vector library for 3d graphics

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vector where

import Prelude
import PolymorphicNum as L
import Data.List
import Misc
import Control.DeepSeq

data Vector = Vector { vecX :: {-# UNPACK #-} !Double,
                       vecY :: {-# UNPACK #-} !Double,
                       vecZ :: {-# UNPACK #-} !Double,
                       vecW :: {-# UNPACK #-} !Double } deriving (Ord, Eq)
type Position = Vector
type Direction = Vector
type Normal = Vector
type TangentSpace = (Vector, Vector, Vector)
type SurfaceLocation = (Position, TangentSpace)

instance PolymorphicNum Vector Vector Vector where
    {-# SPECIALIZE INLINE (<+>) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) <+> (Vector !x' !y' !z' !w') = Vector (x + x') (y + y') (z + z') (w + w')
    {-# SPECIALIZE INLINE (<->) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) <-> (Vector !x' !y' !z' !w') = Vector (x - x') (y - y') (z - z') (w - w')
    {-# SPECIALIZE INLINE (<*>) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) <*> (Vector !x' !y' !z' !w') = Vector (x * x') (y * y') (z * z') (w * w')
    {-# SPECIALIZE INLINE (</>) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) </> (Vector !x' !y' !z' !w') = Vector (x / x') (y / y') (z / z') (w / w')

instance PolymorphicNum Vector Double Vector where
    {-# SPECIALIZE INLINE (<+>) :: Vector -> Double -> Vector #-}
    (Vector !x !y !z !w) <+> (!k) = Vector (x + k) (y + k) (z + k) (w + k)
    {-# SPECIALIZE INLINE (<->) :: Vector -> Double -> Vector #-}
    (Vector !x !y !z !w) <-> (!k) = Vector (x - k) (y - k) (z - k) (w - k)
    {-# SPECIALIZE INLINE (<*>) :: Vector -> Double -> Vector #-}
    (Vector !x !y !z !w) <*> (!k) = Vector (x * k) (y * k) (z * k) (w * k)
    {-# SPECIALIZE INLINE (</>) :: Vector -> Double -> Vector #-}
    (Vector !x !y !z !w) </> (!k) = Vector (x / k) (y / k) (z / k) (w / k)

instance PolymorphicNum Double Vector Vector where
    {-# SPECIALIZE INLINE (<+>) :: Double -> Vector -> Vector #-}
    (!k) <+> (Vector !x !y !z !w) = Vector (k + x) (k + y) (k + z) (k + w)
    {-# SPECIALIZE INLINE (<->) :: Double -> Vector -> Vector #-}
    (!k) <-> (Vector !x !y !z !w) = Vector (k - x) (k - y) (k - z) (k - w)
    {-# SPECIALIZE INLINE (<*>) :: Double -> Vector -> Vector #-}
    (!k) <*> (Vector !x !y !z !w) = Vector (k * x) (k * y) (k * z) (k * w)
    {-# SPECIALIZE INLINE (</>) :: Double -> Vector -> Vector #-}
    (!k) </> (Vector !x !y !z !w) = Vector (k / x) (k / y) (k / z) (k / w)

{-
instance PolymorphicNum Vector (Num a) Vector where
    (Vector x y z w) <+> k = Vector (x + k) (y + k) (z + k) (w + k)
    (Vector x y z w) <-> k = Vector (x - k) (y - k) (z - k) (w - k)
    (Vector x y z w) <*> k = Vector (x * k) (y * k) (z * k) (w * k)
    (Vector x y z w) </> k = Vector (x / k) (y / k) (z / k) (w / k)

instance PolymorphicNum (Num a) Vector Vector where
    k <+> (Vector x y z w) = Vector (k + x) (k + y) (k + z) (k + w)
    k <-> (Vector x y z w) = Vector (k - x) (k - y) (k - z) (k - w)
    k <*> (Vector x y z w) = Vector (k * x) (k * y) (k * z) (k * w)
    k </> (Vector x y z w) = Vector (k / x) (k / y) (k / z) (k / w)
-}

instance Show Vector where
    show (Vector !x !y !z !w) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ show w ++ ")"

instance NFData Vector where
    rnf (Vector x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w

tsTangent :: TangentSpace -> Normal
tsTangent (t, _, _) = t

tsBinormal :: TangentSpace -> Normal
tsBinormal (_, b, _) = b

tsNormal :: TangentSpace -> Normal
tsNormal = thr

xaxis :: Vector
xaxis = Vector 1 0 0 0

yaxis :: Vector
yaxis = Vector 0 1 0 0

zaxis :: Vector
zaxis = Vector 0 0 1 0

waxis :: Vector
waxis = Vector 0 0 0 1

zeroVector :: Vector
zeroVector = Vector 0 0 0 0

setWTo1 :: Vector -> Vector
{-# SPECIALIZE INLINE setWTo1 :: Vector -> Vector #-}
setWTo1 v = v { vecW = 1 }

setWTo0 :: Vector -> Vector
{-# SPECIALIZE INLINE setWTo0 :: Vector -> Vector #-}
setWTo0 v = v { vecW = 0 }

restoreOriginalW :: Vector -> Vector -> Vector
{-# SPECIALIZE INLINE restoreOriginalW :: Vector -> Vector -> Vector #-}
restoreOriginalW (Vector _ _ _ !w') (Vector !x !y !z _) = Vector x y z w'

madd :: Position -> Direction -> Double -> Vector
{-# SPECIALIZE INLINE madd :: Vector -> Vector -> Double -> Vector #-}
madd (Vector !x !y !z !w) (Vector !x' !y' !z' !w') !scalar = Vector x'' y'' z'' w''
    where
      x'' = x + x' * scalar
      y'' = y + y' * scalar
      z'' = z + z' * scalar
      w'' = w + w' * scalar

negate :: Direction -> Direction
{-# SPECIALIZE INLINE Vector.negate :: Vector -> Vector #-}
negate (Vector !x !y !z !w) = Vector (-x) (-y) (-z) (-w)

dot3 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Double #-}
(Vector !x !y !z _) `dot3` (Vector !x' !y' !z' _) = x * x' + y * y' + z * z'

dot4 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot4 :: Vector -> Vector -> Double #-}
(Vector !x !y !z !w) `dot4` (Vector !x' !y' !z' !w') = x * x' + y * y' + z * z' + w * w'

sdot3 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Double #-}
(Vector !x !y !z _) `sdot3` (Vector !x' !y' !z' _) = saturate (x * x' + y * y' + z * z')

sdot4 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE sdot4 :: Vector -> Vector -> Double #-}
(Vector !x !y !z !w) `sdot4` (Vector !x' !y' !z' !w') = saturate (x * x' + y * y' + z * z' + w * w')

cross :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE cross :: Vector -> Vector -> Vector #-}
(Vector !x1 !y1 !z1 _) `cross` (Vector !x2 !y2 !z2 _) = Vector x y z 0
    where
      !x = y1 * z2 - y2 * z1
      !y = z1 * x2 - z2 * x1
      !z = x1 * y2 - x2 * y1

magnitude :: Vector -> Double
{-# SPECIALIZE INLINE magnitude :: Vector -> Double #-}
magnitude !vec = sqrt (magnitudeSq vec)

magnitudeSq :: Vector -> Double
{-# SPECIALIZE INLINE magnitudeSq :: Vector -> Double #-}
magnitudeSq (Vector !x !y !z _) = x * x + y * y + z * z

normalise :: Direction -> Direction
{-# SPECIALIZE INLINE normalise :: Direction -> Direction #-}
normalise !a = setWTo0 (a </> magnitude a)

distance :: Position -> Position -> Double
{-# SPECIALIZE INLINE distance :: Position -> Position -> Double #-}
distance !a !b = magnitude (a <-> b)

distanceSq :: Position -> Position -> Double
{-# SPECIALIZE INLINE distanceSq :: Position -> Position -> Double #-}
distanceSq !a !b = magnitudeSq (a <-> b)

reflect :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE reflect :: Direction -> Direction -> Direction #-}
reflect !incoming !normal = setWTo0 $ (normal <*> (2 * (normal `dot3` incoming))) <-> incoming

refract :: Direction -> Direction -> Double -> Direction
{-# SPECIALIZE INLINE refract :: Vector -> Vector -> Double -> Vector #-}
refract !incoming !normal !eta
    | cosTheta1 > 0.0 = setWTo0 $ (l <*> eta) <+> (normal <*> (eta * cosTheta1 - cosTheta2))
    | otherwise = setWTo0 $ (l <*> eta) <+> (normal <*> (eta * cosTheta1 + cosTheta2))
    where !cosTheta1 = normal `dot3` incoming
          !cosTheta2 = sqrt (1.0 - eta ** 2.0 * (1.0 - cosTheta1 ** 2.0))
          !l = Vector.negate incoming

largestAxis :: Vector -> Int
largestAxis (Vector !x !y !z _) 
    | abs x >= abs y && abs x >= abs z = 0
    | abs y >= abs x && abs y >= abs z = 1
    | abs z >= abs x && abs z >= abs y = 2
    | otherwise = error "largestAxis: Undefined case"

nthLargestAxis :: Vector -> Int -> Int
nthLargestAxis (Vector !x !y !z _) order 
    | order < 3 = snd (sort [(abs x, 0), (abs y, 1), (abs z, 2)] !! order)
    | otherwise = error "nthLargestAXis: Undefined case"

min :: Vector -> Vector -> Vector
{-# SPECIALIZE INLINE Vector.min :: Vector -> Vector -> Vector #-}
min (Vector !x1 !y1 !z1 !w1) (Vector !x2 !y2 !z2 !w2) = Vector x y z w
    where
      !x = Prelude.min x1 x2
      !y = Prelude.min y1 y2
      !z = Prelude.min z1 z2
      !w = Prelude.min w1 w2

max :: Vector -> Vector -> Vector
{-# SPECIALIZE INLINE Vector.max :: Vector -> Vector -> Vector #-}
max (Vector !x1 !y1 !z1 !w1) (Vector !x2 !y2 !z2 !w2) = Vector x y z w
    where
      !x = Prelude.max x1 x2
      !y = Prelude.max y1 y2
      !z = Prelude.max z1 z2
      !w = Prelude.max w1 w2

directionToSpherical :: Direction -> (Double, Double)
directionToSpherical (Vector !x !y !z _) = (theta, phi)
    where
      theta = acos z / pi
      phi = (atan2 y x + pi) / (2.0 * pi)

sphericalToDirection :: Double -> Double -> Direction
sphericalToDirection !theta !phi = Vector (sin theta * cos phi) (sin theta * sin phi) (cos theta) 1

component :: Vector -> Int -> Double
{-# SPECIALIZE INLINE component :: Vector -> Int -> Double #-}
component (Vector !x _ _ _) 0 = x
component (Vector _ !y _ _) 1 = y
component (Vector _ _ !z _) 2 = z
component (Vector _ _ _ !w) 3 = w
component _ _ = error "Invalid component index"

transformDir :: Direction -> TangentSpace -> Direction
{-# SPECIALIZE INLINE transformDir :: Direction -> TangentSpace -> Direction #-}
transformDir (Vector !x !y !z _) !(tangent, binormal, normal) = (setWTo0 . normalise) (tangent <*> x <+> binormal <*> y <+> normal <*> z)

recipPerElem :: Vector -> Vector
recipPerElem (Vector !x !y !z !w) = Vector (f x) (f y) (f z) (f w)
  where
    f a | a == 0 = 10000000 -- What is Haskell for infinity?
        | otherwise = 1 / a
