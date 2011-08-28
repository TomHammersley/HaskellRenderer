-- This defines all unit tests to be executed for this project

import Tests.VectorTest
import Tests.BoundingBoxTest
import Test.HUnit

unitTests = [tests_Vector, tests_BoundingBox]

main = mapM runTestTT unitTests