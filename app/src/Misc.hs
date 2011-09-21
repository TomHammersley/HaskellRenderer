-- Various assorted bits and pieces
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Misc where
import GHC.Prim
import GHC.Types
import Data.List
import Control.Parallel.Strategies
import Control.Monad.State

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
harmonicMean array@(_:_) = fromIntegral (length array) / foldl' (\a b -> b + 1 / a) 0 array
harmonicMean [] = 0

-- This performs a map, and passes through the state of the completed operation to the next recursion
-- Couldn't work out the equivalent using the state monad etc
mapS :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
mapS f z s = mapS' z s []
    where
      mapS' !(x:xs) !st !acc = seq (result, st') $ mapS' xs st' (result : acc)
          where (!result, !st') = f x st `using` rseq
      mapS' [] !st !acc = (acc, st)

-- Map over a list, passing state from one to the next with the state monad
stateMap :: [a] -> s -> (a -> State s b) -> ([b], s)
stateMap arr s f = stateMap' arr s []
    where
      stateMap' (x:xs) st acc = stateMap' xs st' (result : acc)
          where
            (result, st') = runState (f x) st
      stateMap' [] st acc = (acc, st)

