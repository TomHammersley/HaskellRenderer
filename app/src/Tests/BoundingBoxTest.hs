module Tests.BoundingBoxTest where

import Vector
import BoundingBox
import Test.HUnit

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
