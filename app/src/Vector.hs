-- Vector library for 3d graphics

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Vector where
import Data.List
import Misc
import GHC.Prim
import GHC.Types

data Vector = Vector { vecX :: {-# UNPACK #-} !Double,
                       vecY :: {-# UNPACK #-} !Double,
                       vecZ :: {-# UNPACK #-} !Double,
                       vecW :: {-# UNPACK #-} !Double } deriving (Ord, Eq)
type Position = Vector
type Direction = Vector
type Normal = Direction
type TangentSpace = (Normal, Normal, Normal)
type SurfaceLocation = (Position, TangentSpace)

instance Num Vector where
    {-# SPECIALIZE INLINE (+) :: Vector -> Vector -> Vector #-}
    (Vector !(D# x) !(D# y) !(D# z) !(D# w)) + (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = Vector (D# $ x +## x') (D# $ y +## y') (D# $ z +## z') (D# $ w +## w')
    {-# SPECIALIZE INLINE (-) :: Vector -> Vector -> Vector #-}
    (Vector !(D# x) !(D# y) !(D# z) !(D# w)) - (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = Vector (D# $ x -## x') (D# $ y -## y') (D# $ z -## z') (D# $ w -## w')
    {-# SPECIALIZE INLINE (*) :: Vector -> Vector -> Vector #-}
    (Vector !(D# x) !(D# y) !(D# z) !(D# w)) * (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = Vector (D# $ x *## x') (D# $ y *## y') (D# $ z *## z') (D# $ w *## w')
    abs (Vector !x !y !z !w) = Vector absX absY absZ absW
        where
          !absX = abs x
          !absY = abs y
          !absZ = abs z
          !absW = abs w
    signum (Vector !x !y !z !w) = Vector signumX signumY signumZ signumW
        where
          !signumX = signum x
          !signumY = signum y
          !signumZ = signum z
          !signumW = signum w

    fromInteger x = Vector x' x' x' x'
        where
          !x' = fromInteger x

instance Fractional Vector where
    {-# SPECIALIZE INLINE (/) :: Vector -> Vector -> Vector #-}
    (Vector !(D# x) !(D# y) !(D# z) !(D# w)) / (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = Vector (D# $ x /## x') (D# $ y /## y') (D# $ z /## z') (D# $ w /## w')
    fromRational x = Vector x' x' x' x'
        where
          !x' = fromRational x

instance Show Vector where
    show (Vector !x !y !z !w) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ show w ++ ")"

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
setWTo1 (Vector !x !y !z _) = Vector x y z 1

setWTo0 :: Vector -> Vector
{-# SPECIALIZE INLINE setWTo0 :: Vector -> Vector #-}
setWTo0 (Vector !x !y !z _) = Vector x y z 0

restoreOriginalW :: Vector -> Vector -> Vector
{-# SPECIALIZE INLINE restoreOriginalW :: Vector -> Vector -> Vector #-}
restoreOriginalW (Vector _ _ _ !w') (Vector !x !y !z _) = Vector x y z w'

madd :: Position -> Direction -> Double -> Vector
{-# SPECIALIZE INLINE madd :: Vector -> Vector -> Double -> Vector #-}
madd (Vector !(D# x) !(D# y) !(D# z) !(D# w)) (Vector !(D# x') !(D# y') !(D# z') !(D# w')) !(D# scalar) = Vector x'' y'' z'' w''
    where
      x'' = D# (x +## (x' *## scalar))
      y'' = D# (y +## (y' *## scalar))
      z'' = D# (z +## (z' *## scalar))
      w'' = D# (w +## (w' *## scalar))

negate :: Direction -> Direction
{-# SPECIALIZE INLINE Vector.negate :: Vector -> Vector #-}
negate (Vector !x !y !z !w) = Vector (-x) (-y) (-z) (-w)

vectorScalarMul :: Vector -> Double -> Vector
{-# SPECIALIZE INLINE vectorScalarMul :: Vector -> Double -> Vector #-}
(Vector !x !y !z !w) `vectorScalarMul` k = Vector (x * k) (y * k) (z * k) (w * k)

infixl 7 <*>
infixl 7 </>
--infixl 6 <+>
--infixl 6 <->

(</>) :: Vector -> Double -> Vector
{-# SPECIALIZE INLINE (</>) :: Vector -> Double -> Vector #-}
a </> b = a `vectorScalarMul` (1 / b)

(<*>) :: Vector -> Double -> Vector
{-# SPECIALIZE INLINE (<*>) :: Vector -> Double -> Vector #-}
a <*> b = a `vectorScalarMul` b

dot3 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Double #-}
(Vector !(D# x) !(D# y) !(D# z) _) `dot3` (Vector !(D# x') !(D# y') !(D# z') _) = D# $ (x *## x') +## (y *## y') +## (z *## z')

dot4 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot4 :: Vector -> Vector -> Double #-}
(Vector !(D# x) !(D# y) !(D# z) !(D# w)) `dot4` (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = D# $ (x *## x') +## (y *## y') +## (z *## z') +## (w *## w')

sdot3 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Double #-}
(Vector !(D# x) !(D# y) !(D# z) _) `sdot3` (Vector !(D# x') !(D# y') !(D# z') _) = D# $ saturate## ((x *## x') +## (y *## y') +## (z *## z'))

sdot4 :: Vector -> Vector -> Double
{-# SPECIALIZE INLINE sdot4 :: Vector -> Vector -> Double #-}
(Vector !(D# x) !(D# y) !(D# z) !(D# w)) `sdot4` (Vector !(D# x') !(D# y') !(D# z') !(D# w')) = D# $ saturate## ((x *## x') +## (y *## y') +## (z *## z') +## (w *## w'))

cross :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE cross :: Vector -> Vector -> Vector #-}
(Vector !(D# x1) !(D# y1) !(D# z1) _) `cross` (Vector !(D# x2) !(D# y2) !(D# z2) _) = Vector x y z 0
    where
      !x = D# ((y1 *## z2) -## (y2 *## z1))
      !y = D# ((z1 *## x2) -## (z2 *## x1))
      !z = D# ((x1 *## y2) -## (x2 *## y1))

magnitude :: Vector -> Double
{-# SPECIALIZE INLINE magnitude :: Vector -> Double #-}
magnitude !vec = sqrt (magnitudeSq vec)

magnitudeSq :: Vector -> Double
{-# SPECIALIZE INLINE magnitudeSq :: Vector -> Double #-}
magnitudeSq (Vector !(D# x#) !(D# y#) !(D# z#) _) = D# ((x# *## x#) +## (y# *## y#) +## (z# *## z#))

normalise :: Direction -> Direction
{-# SPECIALIZE INLINE normalise :: Direction -> Direction #-}
normalise !a = setWTo0 (a `vectorScalarMul` (1 / magnitude a))

distance :: Position -> Position -> Double
{-# SPECIALIZE INLINE distance :: Position -> Position -> Double #-}
distance !a !b = magnitude (a - b)

distanceSq :: Position -> Position -> Double
{-# SPECIALIZE INLINE distanceSq :: Position -> Position -> Double #-}
distanceSq !a !b = magnitudeSq (a - b)

reflect :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE reflect :: Direction -> Direction -> Direction #-}
reflect !incoming !normal = restoreOriginalW incoming $ (normal `vectorScalarMul` (2 * (normal `dot3` incoming))) - incoming

refract :: Direction -> Direction -> Double -> Direction
{-# SPECIALIZE INLINE refract :: Vector -> Vector -> Double -> Vector #-}
refract !incoming !normal !eta
    | cosTheta1 >## 0.0## = setWTo0 $ (l `vectorScalarMul` eta) + (normal `vectorScalarMul` D# (eta# *## cosTheta1 -## cosTheta2))
    | otherwise = setWTo0 $ (l `vectorScalarMul` eta) + (normal `vectorScalarMul` D# (eta# *## cosTheta1 +## cosTheta2))
    where !(D# cosTheta1) = normal `dot3` incoming
          !cosTheta2 = sqrtDouble# (1.0## -## eta# **## 2.0## *## (1.0## -## cosTheta1 **## 2.0##))
          !l = Vector.negate incoming
          !(D# eta#) = eta

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
sphericalToDirection (D# !theta) (D# !phi) = Vector (D# $ sinDouble# theta *## cosDouble# phi) (D# $ sinDouble# theta *## sinDouble# phi) (D# $ cosDouble# theta) 1

component :: Vector -> Int -> Double
{-# SPECIALIZE INLINE component :: Vector -> Int -> Double #-}
component (Vector !x _ _ _) 0 = x
component (Vector _ !y _ _) 1 = y
component (Vector _ _ !z _) 2 = z
component (Vector _ _ _ !w) 3 = w
component _ _ = error "Invalid component index"

transformDir :: Direction -> TangentSpace -> Direction
{-# SPECIALIZE INLINE transformDirvecto :: Direction -> TangentSpace -> Direction #-}
transformDir !dir !(tangent, binormal, normal) = Vector (dir `dot3` tangent) (dir `dot3` binormal) (dir `dot3` normal) 0
