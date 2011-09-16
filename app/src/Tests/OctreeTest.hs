module Tests.OctreeTest where

import Test.HUnit
import Vector
import Octree

test_create = TestCase (assertEqual "create" expectedResult ((create box) :: OctTree Int))
    where
      box = (Vector (-10) (-10) (-10) 1, Vector 10 10 10 1)
      octChildren = [
          (OctTreeDummy (Vector (-10) (-10) (-10) 1, Vector 0 0 0 1)) :: OctTree Int,
          (OctTreeDummy (Vector 0 (-10) (-10) 1, Vector 10 0 0 1)) :: OctTree Int,
          (OctTreeDummy (Vector (-10) 0 (-10) 1, Vector 0 10 0 1)) :: OctTree Int,
          (OctTreeDummy (Vector 0 0 (-10) 1, Vector 10 10 0 1)) :: OctTree Int,
          (OctTreeDummy (Vector (-10) (-10) 0 1, Vector 0 0 10 1)) :: OctTree Int,
          (OctTreeDummy (Vector 0 (-10) 0 1, Vector 10 0 10 1)) :: OctTree Int,
          (OctTreeDummy (Vector (-10) 0 0 1, Vector 0 10 10 1)) :: OctTree Int,
          (OctTreeDummy (Vector 0 0 0 1, Vector 10 10 10 1)) :: OctTree Int
          ]
      expectedResult = (OctTreeNode (Vector (-10) (-10) (-10) 1, Vector 10 10 10 1) octChildren) :: OctTree Int

tests_Octree = TestList [
                TestLabel "Creation" test_create
               ]
