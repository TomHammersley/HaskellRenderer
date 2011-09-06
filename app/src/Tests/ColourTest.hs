module Tests.ColourTest where

import Colour
import Test.HUnit

test_Add = TestCase (assertEqual "Colour addition" expected (v1 + v2))
    where
      v1 = Colour 1 2 3 4
      v2 = Colour 10 20 30 40
      expected = Colour 11 22 33 44

test_Sub = TestCase (assertEqual "Colour subtraction" expected (v1 - v2))
    where
      v1 = Colour 10 20 30 40
      v2 = Colour 5 10 15 20
      expected = Colour 5 10 15 20

test_Mul = TestCase (assertEqual "Colour multiplication" expected (v1 * v2))
    where
      v1 = Colour 10 20 30 40
      v2 = Colour 5 10 15 20
      expected = Colour 50 200 450 800

test_Abs = TestCase (assertEqual "Colour abs" expected (abs v1))
    where
      v1 = Colour 10 (-20) 0 (-40)
      expected = Colour 10 20 0 40

test_Signum = TestCase (assertEqual "Colour signum" expected (signum v1))
    where
      v1 = Colour 10 (-20) 0 (-40)
      expected = Colour 1 (-1) 0 (-1)

test_fromInteger = TestCase (assertEqual "Colour fromInteger" expected (signum v1))
    where
      v1 = fromInteger 5
      expected = Colour 5 5 5 5

test_Div = TestCase (assertEqual "Colour division" expected (v1 / v2))
    where
      v1 = Colour 10 20 30 40
      v2 = Colour 5 10 15 20
      expected = Colour 2 2 2 2

test_AddScalar = TestCase (assertEqual "Colour add scalar" expected (v1 <+> k))
    where
      v1 = Colour 10 20 30 40
      k = 1
      expected = Colour 11 21 31 41

test_SubScalar = TestCase (assertEqual "Colour sub scalar" expected (v1 <-> k))
    where
      v1 = Colour 10 20 30 40
      k = 1
      expected = Colour 9 19 29 39

test_Clamp = TestCase (assertEqual "Colour clamp" expected (clamp v1))
    where
      v1 = Colour 2 0 (-5) 0.5
      expected = Colour 1 0 0 0.5

test_Luminance0 = TestCase (assertEqual "Luminance = 0" expected (luminance v1))
    where
      v1 = Colour 0 0 0 0
      expected = 0

test_Luminance1 = TestCase (assertEqual "Luminance = 1" expected (luminance v1))
    where
      v1 = Colour 1 1 1 1
      expected = 0.9999999999999999

tests_Colour = TestList [
                TestLabel "Addition" test_Add,
                TestLabel "Subtraction" test_Sub,
                TestLabel "Multiplication" test_Mul,
                TestLabel "Abs" test_Abs,
                TestLabel "Signum" test_Signum,
                TestLabel "Div" test_Div,
                TestLabel "Add scalar" test_AddScalar,
                TestLabel "Sub scalar" test_SubScalar,
                TestLabel "Clamp" test_Clamp,
                TestLabel "Luminance = 0" test_Luminance0,
                TestLabel "Luminance = 1" test_Luminance1
               ]