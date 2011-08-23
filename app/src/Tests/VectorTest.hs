module Tests.VectorTest where

import Vector
import Test.HUnit

test_Add = TestCase (assertEqual "Vector addition" (v1 + v2) expectedResult)
    where
      v1 = Vector 1 2 3 4
      v2 = Vector 10 20 30 40
      expectedResult = Vector 11 22 33 44

test_Sub = TestCase (assertEqual "Vector subtraction" (v1 - v2) expectedResult)
    where
      v1 = Vector 10 20 30 40
      v2 = Vector 1 2 3 4
      expectedResult = Vector 9 18 27 36

test_Mul = TestCase (assertEqual "Vector multiplication" (v1 * v2) expectedResult)
    where
      v1 = Vector 1 0 2 3
      v2 = Vector 1 10 (-2) 3
      expectedResult = Vector 1 0 (-4) 9

test_Negate = TestCase (assertEqual "Vector negation" (Vector.negate v1) expectedResult)
    where
      v1 = Vector 1 (-2) 3 (-4)
      expectedResult = Vector (-1) 2 (-3) 4

test_Abs = TestCase (assertEqual "Vector abs" (abs v1) expectedResult)
    where
      v1 = Vector 0 1 (-2) 3
      expectedResult = Vector 0 1 2 3

test_Signum = TestCase (assertEqual "Vector signum" (signum v1) expectedResult)
    where
      v1 = Vector 1 (-1) 0 (-2)
      expectedResult = Vector 1 (-1) 0 (-1)

test_Madd = TestCase (assertEqual "Vector madd" (madd pos dir k) expectedResult)
    where
      pos = Vector 1 2 3 1
      dir = Vector 0.5 0 1 0
      k = 10
      expectedResult = Vector 6 2 13 1

test_ScalarMul = TestCase (assertEqual "Vector-scalar mul" (vectorScalarMul vec k) expectedResult)
    where
      vec = Vector 1 2 (-3) 1
      k = 2
      expectedResult = Vector 2 4 (-6) 2

test_ScalarDiv = TestCase (assertEqual "Vector-scalar div" (vec </> k) expectedResult)
    where
      vec = Vector 10 20 (-30) 40
      k = 2
      expectedResult = Vector 5 10 (-15) 20

test_ScalarDot3 = TestCase (assertEqual "dot3" (v1 `dot3` v2) expectedResult)
    where
      v1 = Vector 1 2 0 1
      v2 = Vector (-2) 4 (-5) 1
      expectedResult = 6

test_ScalarDot4 = TestCase (assertEqual "dot4" (v1 `dot4` v2) expectedResult)
    where
      v1 = Vector 1 2 0 1
      v2 = Vector (-2) 4 (-5) 1
      expectedResult = 7

test_SatScalarDot3 = TestCase (assertEqual "sdot3" (v1 `sdot3` v2) expectedResult)
    where
      v1 = Vector 1 2 0 1
      v2 = Vector (-2) 4 (-5) 1
      expectedResult = 1

test_SatScalarDot4 = TestCase (assertEqual "sdot4" (v1 `sdot4` v2) expectedResult)
    where
      v1 = Vector 1 2 0 1
      v2 = Vector (-2) (-4) (-5) 1
      expectedResult = 0

test_Cross = TestCase (assertEqual "cross" (v1 `cross` v2) expectedResult)
    where
      v1 = Vector 1 0 0 0
      v2 = Vector 0 1 0 0
      expectedResult = Vector 0 0 1 0

test_Magnitude = TestCase (assertEqual "magnitude" (magnitude v1) expectedResult)
    where
      v1 = Vector 3 4 0 0
      expectedResult = 5

test_MagnitudeSq = TestCase (assertEqual "magnitudeSq" (magnitudeSq v1) expectedResult)
    where
      v1 = Vector 3 4 0 0
      expectedResult = 25

test_Normalise = TestCase (assertEqual "normalise" (normalise v1) expectedResult)
    where
      v1 = Vector 1 (-1) 1 0
      expectedResult = Vector 0.5773502691896258 (-0.5773502691896258) 0.5773502691896258 0

test_Reflect = undefined
test_Refract = undefined

test_LargestAxis = TestCase (assertEqual "largestAxis" (largestAxis v1) expectedResult)
    where
      v1 = Vector (-1) 2 (-3) 0
      expectedResult = 2

test_Min = TestCase (assertEqual "min" (v1 `Vector.min` v2) expectedResult)
    where
      v1 = Vector (-1) 2 (-3) 8
      v2 = Vector 10 (-20) 50 2
      expectedResult = Vector (-1) (-20) (-3) 2

test_Max = TestCase (assertEqual "max" (v1 `Vector.max` v2) expectedResult)
    where
      v1 = Vector (-1) 2 (-3) 8
      v2 = Vector 10 (-20) 50 2
      expectedResult = Vector 10 2 50 8

tests_Vector = TestList [
                TestLabel "Addition" test_Add, 
                TestLabel "Subtraction" test_Sub,
                TestLabel "Multiplication" test_Mul,
                TestLabel "Negation" test_Negate,
                TestLabel "Abs" test_Abs,
                TestLabel "Signum" test_Signum,
                TestLabel "Madd" test_Madd,
                TestLabel "Vector * scalar" test_ScalarMul,
                TestLabel "Vector / scalar" test_ScalarDiv,
                TestLabel "Dot3" test_ScalarDot3,
                TestLabel "Dot4" test_ScalarDot4,
                TestLabel "Sdot3" test_SatScalarDot3,
                TestLabel "Sdot4" test_SatScalarDot4,
                TestLabel "Cross" test_Cross,
                TestLabel "Magnitude" test_Magnitude,
                TestLabel "MagnitudeSq" test_MagnitudeSq,
                TestLabel "Normalise" test_Normalise,
                TestLabel "LargestAxis" test_LargestAxis,
                TestLabel "Min" test_Min,
                TestLabel "Max" test_Max
               ]
