{-# LANGUAGE BangPatterns #-}

module Vector where
import Data.List
import Misc

data Vector = Vector { vecX :: {-# UNPACK #-} !Float,
                       vecY :: {-# UNPACK #-} !Float,
                       vecZ :: {-# UNPACK #-} !Float,
                       vecW :: {-# UNPACK #-} !Float } deriving (Read, Ord, Eq)
type Position = Vector
type Direction = Vector
type Normal = Direction
type TangentSpace = (Normal, Normal, Normal)

instance Num Vector where
    {-# SPECIALIZE INLINE (+) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) + (Vector !x' !y' !z' !w') = Vector (x + x') (y + y') (z + z') (w + w')
    {-# SPECIALIZE INLINE (-) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) - (Vector !x' !y' !z' !w') = Vector (x - x') (y - y') (z - z') (w - w')
    {-# SPECIALIZE INLINE (*) :: Vector -> Vector -> Vector #-}
    (Vector !x !y !z !w) * (Vector !x' !y' !z' !w') = Vector (x * x') (y * y') (z * z') (w * w')
    abs (Vector x y z w) = Vector (abs x) (abs y) (abs z) (abs w)
    signum (Vector x y z w) = Vector (signum x) (signum y) (signum z) (signum w)
    fromInteger x = Vector (fromInteger x) (fromInteger x) (fromInteger x) (fromInteger x)

instance Show Vector where
    show (Vector x y z w) = "(" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ ", " ++ (show w) ++ ")"

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

madd :: Position -> Direction -> Float -> Vector
{-# SPECIALIZE INLINE madd :: Vector -> Vector -> Float -> Vector #-}
madd (Vector !x !y !z !w) (Vector !x' !y' !z' _) !scalar = Vector (x + x' * scalar) (y + y' * scalar) (z + z' * scalar) w

negate :: Direction -> Direction
{-# SPECIALIZE INLINE Vector.negate :: Vector -> Vector #-}
negate (Vector !x !y !z !w) = Vector (-x) (-y) (-z) w

vectorScalarMul :: Vector -> Float -> Vector
{-# SPECIALIZE INLINE vectorScalarMul :: Vector -> Float -> Vector #-}
(Vector !x !y !z !w) `vectorScalarMul` k = Vector (x * k) (y * k) (z * k) w

(</>) :: Vector -> Float -> Vector
a </> b = a `vectorScalarMul` (1.0 / b)

(<*>) :: Vector -> Float -> Vector
a <*> b = a `vectorScalarMul` b

dot3 :: Vector -> Vector -> Float
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Float #-}
(Vector !x !y !z _) `dot3` (Vector !x' !y' !z' _) = x * x' + y * y' + z * z'

dot4 :: Vector -> Vector -> Float
{-# SPECIALIZE INLINE dot4 :: Vector -> Vector -> Float #-}
(Vector !x !y !z w) `dot4` (Vector !x' !y' !z' w') = x * x' + y * y' + z * z' + w * w'

sdot3 :: Vector -> Vector -> Float
{-# SPECIALIZE INLINE dot3 :: Vector -> Vector -> Float #-}
(Vector !x !y !z _) `sdot3` (Vector !x' !y' !z' _) = saturate (x * x' + y * y' + z * z')

sdot4 :: Vector -> Vector -> Float
{-# SPECIALIZE INLINE sdot4 :: Vector -> Vector -> Float #-}
(Vector !x !y !z w) `sdot4` (Vector !x' !y' !z' w') = saturate (x * x' + y * y' + z * z' + w * w')

cross :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE cross :: Vector -> Vector -> Vector #-}
(Vector !x1 !y1 !z1 _) `cross` (Vector !x2 !y2 !z2 _) = Vector (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1) 0

magnitude :: Vector -> Float
{-# SPECIALIZE INLINE magnitude :: Vector -> Float #-}
magnitude !vec = sqrt(magnitudeSq vec)

magnitudeSq :: Vector -> Float
{-# SPECIALIZE INLINE magnitudeSq :: Vector -> Float #-}
magnitudeSq (Vector !x !y !z _) = x * x + y * y + z * z

normalise :: Direction -> Direction
{-# SPECIALIZE INLINE normalise :: Vector -> Vector #-}
normalise a = setWTo0 (a `vectorScalarMul` (1.0 / magnitude a))

distance :: Position -> Position -> Float
{-# SPECIALIZE INLINE distance :: Vector -> Vector -> Float #-}
distance !a !b = magnitude (a - b)

distanceSq :: Position -> Position -> Float
{-# SPECIALIZE INLINE distanceSq :: Vector -> Vector -> Float #-}
distanceSq !a !b = magnitudeSq (a - b)

reflect :: Direction -> Direction -> Direction
{-# SPECIALIZE INLINE reflect :: Vector -> Vector -> Vector #-}
reflect !incoming !normal = restoreOriginalW incoming $ (normal `vectorScalarMul` (2 * (normal `dot3` incoming))) - incoming

refract :: Direction -> Direction -> Float -> Direction
{-# SPECIALIZE INLINE refract :: Vector -> Vector -> Float -> Vector #-}
refract !incoming !normal !eta
    | cosTheta1 > 0.0 = setWTo0 $ (l `vectorScalarMul` eta) + (normal `vectorScalarMul` (eta * cosTheta1 - cosTheta2))
    | otherwise       = setWTo0 $ (l `vectorScalarMul` eta) + (normal `vectorScalarMul` (eta * cosTheta1 + cosTheta2))
    where cosTheta1 = normal `dot3` incoming
          cosTheta2 = sqrt(1 - eta**2 * (1 - cosTheta1**2))
          l = Vector.negate incoming

largestAxis :: Vector -> Int
largestAxis (Vector x y z _) 
    | abs x >= abs y && abs x >= abs z = 0
    | abs y >= abs x && abs y >= abs z = 1
    | abs z >= abs x && abs z >= abs y = 2
    | otherwise = error "largestAxis: Undefined case"

nthLargestAxis :: Vector -> Int -> Int
nthLargestAxis (Vector x y z _) order 
    | order < 3 = snd (sort [(abs x, 0), (abs y, 1), (abs z, 2)] !! order)
    | otherwise = error "nthLargestAXis: Undefined case"

min :: Vector -> Vector -> Vector
min (Vector !x1 !y1 !z1 !w1) (Vector !x2 !y2 !z2 !w2) = Vector (Prelude.min x1 x2) (Prelude.min y1 y2) (Prelude.min z1 z2) (Prelude.min w1 w2)

max :: Vector -> Vector -> Vector
max (Vector !x1 !y1 !z1 !w1) (Vector !x2 !y2 !z2 !w2) = Vector (Prelude.max x1 x2) (Prelude.max y1 y2) (Prelude.max z1 z2) (Prelude.max w1 w2)

directionToSpherical :: Direction -> (Float, Float)
directionToSpherical (Vector x y z _) = (theta, phi)
    where
      theta = acos z / pi
      phi = (atan2 y x + pi) / (2 * pi)

sphericalToDirection :: (Float, Float) -> Direction
sphericalToDirection (theta, phi) = Vector (sin theta * cos phi) (sin theta * sin phi) (cos theta) 1

component :: Vector -> Int -> Float
{-# SPECIALIZE INLINE component :: Vector -> Int -> Float #-}
component (Vector x _ _ _) 0 = x
component (Vector _ y _ _) 1 = y
component (Vector _ _ z _) 2 = z
component (Vector _ _ _ w) 3 = w
component _ _ = error "Invalid component index"

transformDir :: Direction -> TangentSpace -> Direction
transformDir !dir !(tangent, binormal, normal) = Vector (dir `dot3` tangent) (dir `dot3` binormal) (dir `dot3` normal) 0
