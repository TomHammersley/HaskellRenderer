-- This defines all unit tests to be executed for this project

import Tests.VectorTest
import Tests.BoundingBoxTest
import Tests.ColourTest
import Test.HUnit

unitTests = [tests_Vector, tests_BoundingBox, tests_Colour]

main = mapM runTestTT unitTests