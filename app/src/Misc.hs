-- Various assorted bits and pieces
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Misc where

import GHC.Prim
import GHC.Types
import Data.List
import Control.Parallel.Strategies
import Control.Monad.State
import System.Random

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

-- Map over a list, passing state from one to the next with the state monad and returning state
mapWithState :: [a] -> s -> (a -> State s b) -> ([b], s)
mapWithState arr s f = mapWithState' arr s []
    where
      mapWithState' (x:xs) st acc = mapWithState' xs st' (result : acc)
          where
            (result, st') = runState (f x) st
      mapWithState' [] st acc = (acc, st)

-- As above, but discard state
mapWithStateDiscard :: [a] -> s -> (a -> State s b) -> [b]
mapWithStateDiscard arr s f = mapWithState' arr s []
    where
      mapWithState' (x:xs) st acc = mapWithState' xs st' (result : acc)
          where
            (result, st') = runState (f x) st
      mapWithState' [] _ acc = acc

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

zipWithState3 :: (a -> b -> c -> State s d) -> [a] -> [b] -> [c] -> s -> ([d], s)
zipWithState3 f arr1 arr2 arr3 s = zipWithState3' arr1 arr2 arr3 s []
    where
      zipWithState3' (x:xs) (y:ys) (z:zs) st acc = zipWithState3' xs ys zs st' (result : acc)
          where
            (result, st') = runState (f x y z) st
      zipWithState3' (_:_) [] _ st acc = (acc, st)
      zipWithState3' (_:_) (_:_) [] st acc = (acc, st)
      zipWithState3' [] (_:_) _ st acc = (acc, st)
      zipWithState3' [] [] (_:_) st acc = (acc, st)
      zipWithState3' [] [] [] st acc = (acc, st)

zipWith' :: (a -> b -> t) -> [a] -> [b] -> [t]
zipWith' f l1 l2 = [ f e1 e2 | (e1, e2) <- zipWith k l1 l2 ]
    where
      k x y = x `seq` y `seq` (x,y)

-- Repeatedly call a function and pass state (eg, random numbers)
replicateWithState :: Int -> s -> (State s b) -> ([b], s)
replicateWithState count s f = replicateWithState' count s []
    where
      replicateWithState' 0 st acc = (acc, st)
      replicateWithState' ct st acc = replicateWithState' (ct - 1) st' (result : acc)
          where
            (result, st') = runState f st

randDouble :: (RandomGen g) => State g Double
randDouble = do
  gen <- get
  let (r, gen') = randomR (0, 1) gen
  put gen'
  return r

-- Handy little thing to apply a different function to each of the two elements in a Maybe (a, a) pair. Useful in various ray tracing bits of code
maybePairFunctor :: (Ord a) => (a -> a -> a) -> (a -> a -> a) -> Maybe (a, a) -> Maybe (a, a) -> Maybe (a, a)
maybePairFunctor _ _ Nothing Nothing = Nothing
maybePairFunctor _ _ Nothing x@(Just (_, _)) = x
maybePairFunctor _ _ x@(Just (_, _)) Nothing = x
maybePairFunctor f1 f2 (Just (a1, a2)) (Just (b1, b2)) = Just (a1 `f1` b1, a2 `f2` b2)
