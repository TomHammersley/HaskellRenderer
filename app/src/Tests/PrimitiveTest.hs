{-# LANGUAGE UnboxedTuples #-}

module Tests.PrimitiveTest where

import PolymorphicNum
import Vector
import Test.HUnit
import Primitive
import Ray

test_triIntersect1 = TestCase (assertEqual "Triangle intersection 1" expectedResult actualResult)
    where
      v1 = Vector 0 0 0 1
      v2 = Vector 10 10 0 1
      v3 = Vector 20 0 0 1
      tri = makeTriangle v1 v2 v3
      ray = rayWithDirection (Vector 10 5 (-10) 1) (Vector 0 0 1 0) 1000
      expectedResult = True
      (# actualResult, _, _ #) = (intersectRayTriangle ray tri True)

test_triIntersect2 = TestCase (assertEqual "Triangle intersection 2" expectedResult actualResult)
    where
      v1 = Vector 0 0 0 1
      v2 = Vector 10 10 0 1
      v3 = Vector 20 0 0 1
      tri = makeTriangle v1 v2 v3
      ray = rayWithDirection (Vector (-100) 5 (-10) 1) (Vector 0 0 1 0) 1000
      expectedResult = False
      (# actualResult, _, _ #) = (intersectRayTriangle ray tri True)

test_triIntersect3 = TestCase (assertEqual "Triangle intersection 3" expectedResult actualResult)
    where
      v1 = Vector 0 0 0 1
      v2 = Vector 10 10 0 1
      v3 = Vector 20 0 0 1
      tri = makeTriangle v1 v2 v3
      ray = rayWithDirection (Vector 100 5 (-10) 1) (Vector 0 0 1 0) 1000
      expectedResult = False
      (# actualResult, _, _ #) = (intersectRayTriangle ray tri True)

test_triIntersect4 = TestCase (assertEqual "Triangle intersection 4" expectedResult actualResult)
    where
      v1 = Vector 0 0 0 1
      v2 = Vector 10 10 0 1
      v3 = Vector 20 0 0 1
      tri = makeTriangle v1 v2 v3
      ray = rayWithDirection (Vector 10 500 (-10) 1) (Vector 0 0 1 0) 1000
      expectedResult = False
      (# actualResult, _, _ #) = (intersectRayTriangle ray tri True)

test_triIntersect5 = TestCase (assertEqual "Triangle intersection 5" expectedResult actualResult)
    where
      v1 = Vector 0 0 0 1
      v2 = Vector 10 10 0 1
      v3 = Vector 20 0 0 1
      tri = makeTriangle v1 v2 v3
      ray = rayWithDirection (Vector 10 (-500) (-10) 1) (Vector 0 0 1 0) 1000
      expectedResult = False
      (# actualResult, _, _ #) = (intersectRayTriangle ray tri True)

tests_Primitive = TestList [
                TestLabel "Triangle intersection 1" test_triIntersect1,
                TestLabel "Triangle intersection 2" test_triIntersect2,
                TestLabel "Triangle intersection 3" test_triIntersect3,
                TestLabel "Triangle intersection 4" test_triIntersect4,
                TestLabel "Triangle intersection 5" test_triIntersect5
              ]
