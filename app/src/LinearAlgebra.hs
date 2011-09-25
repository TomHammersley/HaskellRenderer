-- Module for a generic typeclass to bind together my linear algebra maths - vectors, matrices - rather than instancing off Num
{-# LANGUAGE MultiParamTypeClasses #-}

module LinearAlgebra where

class LinearAlgebra a b c where
    (*) :: a -> b -> c
    (/) :: a -> b -> c
    (-) :: a -> b -> c
    (+) :: a -> b -> c
