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
zipWithState f arr1 arr2 s = zipWithState' arr1 arr2 s []
    where
      zipWithState' (x:xs) (y:ys) st acc = zipWithState' xs ys st' (result : acc)
          where
            (result, st') = runState (f x y) st
      zipWithState' (_:_) [] _ _ = error "Lists are of a different size - unhandled case!"
      zipWithState' [] (_:_) _ _ = error "Lists are of a different size - unhandled case!"
      zipWithState' [] [] st acc = (acc, st)

zipWith' :: (a -> b -> t) -> [a] -> [b] -> [t]
zipWith' f l1 l2 = [ f e1 e2 | (e1, e2) <- zipWith k l1 l2 ]
    where
      k x y = x `seq` y `seq` (x,y)
