-- Various assorted bits and pieces
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Misc where
import GHC.Prim
import GHC.Types
import Data.List

degreesToRadians :: Double -> Double
degreesToRadians x = x * pi / 180

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

thr :: (x, y, z) -> z
thr (_, _, c) = c

-- Little helper for saturation
saturate :: (Num t, Ord t) => t -> t
saturate x = Prelude.max 0 (Prelude.min x 1)

saturate## :: Double# -> Double#
saturate## !x = value
    where
      !(D# !value) = Prelude.max 0 (Prelude.min (D# x) 1)

harmonicMean :: (Num t, Fractional t) => [t] -> t
harmonicMean (x:xs) = fromIntegral (length (x:xs)) / foldl' (\a b -> b + 1 / a) 0 (x:xs)
harmonicMean [] = 0
