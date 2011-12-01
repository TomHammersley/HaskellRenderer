module Tests.OctreeTest where

import Test.HUnit
import Vector
import Octree

test_create = TestCase (assertEqual "create" expectedResult ((create box) :: Octree Int))
    where
      box = (Vector (-10) (-10) (-10) 1, Vector 10 10 10 1)
      octChildren = [
          (OctreeDummy (Vector (-10) (-10) (-10) 1, Vector 0 0 0 1)) :: Octree Int,
          (OctreeDummy (Vector 0 (-10) (-10) 1, Vector 10 0 0 1)) :: Octree Int,
          (OctreeDummy (Vector (-10) 0 (-10) 1, Vector 0 10 0 1)) :: Octree Int,
          (OctreeDummy (Vector 0 0 (-10) 1, Vector 10 10 0 1)) :: Octree Int,
          (OctreeDummy (Vector (-10) (-10) 0 1, Vector 0 0 10 1)) :: Octree Int,
          (OctreeDummy (Vector 0 (-10) 0 1, Vector 10 0 10 1)) :: Octree Int,
          (OctreeDummy (Vector (-10) 0 0 1, Vector 0 10 10 1)) :: Octree Int,
          (OctreeDummy (Vector 0 0 0 1, Vector 10 10 10 1)) :: Octree Int
          ]
      expectedResult = (OctreeNode (Vector (-10) (-10) (-10) 1, Vector 10 10 10 1) octChildren) :: Octree Int

tests_Octree = TestList [
                TestLabel "Creation" test_create
               ]
