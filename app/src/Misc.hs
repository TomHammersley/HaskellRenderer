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
mapWithState :: [a] -> s -> (a -> State s b) -> ([b], s)
mapWithState arr s f = mapWithState' arr s []
    where
      mapWithState' (x:xs) st acc = mapWithState' xs st' (result : acc)
          where
            (result, st') = runState (f x) st
      mapWithState' [] st acc = (acc, st)

-- Zip over two lists, passing state from one to the next with the state monad
zipWithState :: (a -> b -> State s c) -> [a] -> [b] -> s -> ([c], s)
zipWithState f arr1 arr2 s = mapWithState' arr1 arr2 s []
    where
      mapWithState' (x:xs) (y:ys) st acc = mapWithState' xs ys st' (result : acc)
          where
            (result, st') = runState (f x y) st
      mapWithState' (_:_) [] _ _ = error "Lists are of a different size - unhandled case!"
      mapWithState' [] (_:_) _ _ = error "Lists are of a different size - unhandled case!"
      mapWithState' [] [] st acc = (acc, st)

