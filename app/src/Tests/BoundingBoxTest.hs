module Tests.BoundingBoxTest where

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

import Test.HUnit
import Vector
import BoundingBox
import GHC.Prim
import GHC.Types

test_InsideBox = TestCase (assertEqual "Inside box" (box `contains` pos) True)
    where
      box = (Vector (-5) (-5) (-5) 1, Vector 5 5 5 1)
      pos = Vector 0 0 0 1

test_OutsideBoxMinX = TestCase (assertEqual "Inside box" (box `contains` pos) False)
    where
      box = (Vector (-5) (-5) (-5) 1, Vector 5 5 5 1)
      pos = Vector (-10) 0 0 1

tests_BoundingBox = TestList [
                     TestLabel "InsideBox" test_InsideBox,
                     TestLabel "OutsideBoxMinX" test_OutsideBoxMinX
                    ]
