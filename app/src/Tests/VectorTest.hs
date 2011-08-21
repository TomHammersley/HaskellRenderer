module Tests.VectorTest where

import Vector
import Test.HUnit

tests_Vector = tests_BasicVectorArithmetic

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

tests_BasicVectorArithmetic = TestList [
                               TestLabel "Addition" test_Add, 
                               TestLabel "Subtraction" test_Sub,
                               TestLabel "Multiplication" test_Mul,
                               TestLabel "Negation" test_Negate,
                               TestLabel "Abs" test_Abs,
                               TestLabel "Signum" test_Signum
                              ]

test_W1 = undefined
test_W0 = undefined
test_Madd = undefined
test_ScalarMul = undefined
test_ScalarDiv = undefined
test_ScalarDot3 = undefined
test_ScalarDot4 = undefined
test_SatScalarDot3 = undefined
test_SatScalarDot4 = undefined
test_Cross = undefined
test_Magnitude = undefined
test_MagnitudeSq = undefined
test_Normalise = undefined
test_Reflect = undefined
test_Refract = undefined
test_LargestAxis = undefined
test_Min = undefined
test_Max = undefined
