-- Module for a generic typeclass to bind together my linear algebra maths - vectors, matrices - rather than instancing off Num

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module PolymorphicNum where

class PolymorphicNum a b c | a b -> c where
    (<*>) :: a -> b -> c
    (</>) :: a -> b -> c
    (<->) :: a -> b -> c
    (<+>) :: a -> b -> c
    infixl 7 <*>
    infixl 7 </>
    infixl 6 <+>
    infixl 6 <->
    