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

test_Negate = undefined
test_Abs = undefined
test_Signum = undefined
tests_BasicVectorArithmetic = TestList [
                               TestLabel "Addition" test_Add, 
                               TestLabel "Subtraction" test_Sub,
                               TestLabel "Multiplication" test_Mul
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
