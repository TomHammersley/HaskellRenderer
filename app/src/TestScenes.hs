-- Just some built-in test scenes for debugging purposes

module TestScenes where

import Colour
import Light
import SparseVoxelOctree
import Vector
import BoundingBox
import Primitive
import Matrix
import Material
import Camera

testSvo :: SparseOctree
testSvo = build (sphereOverlapsBox spherePos sphereRadius) (sphereContainsPoint spherePos sphereRadius) (Vector 140 140 190 1, Vector 360 360 410 1) maxRecursion minDimension
  where
    spherePos = Vector 250 250 300 1
    sphereRadius = 100
    maxRecursion = 100
    minDimension = 5
    sphereOverlapsBox pos r box 
      | overlapsSphere box pos r = 1
      | otherwise = 0
    sphereContainsPoint pos r p | p `distance` pos <= r = 1
                                | otherwise = 0

svoTestScene :: [Object]
svoTestScene = [Object (SparseOctreeModel testSvo) defaultMaterial identity]

svoLeafBoxes :: [Object]
svoLeafBoxes = map (\x -> Object (Box x) defaultMaterial identity) (enumerateLeafBoxes testSvo)

boxTestScene :: [Object]
boxTestScene = [Object (Box (Vector (-50) (-50) (-50) 0, Vector 50 50 50 0)) defaultMaterial identity]

testSceneCamera :: Camera
testSceneCamera = withVectors (Vector 0 0 (-400.0) 1.0) xaxis yaxis zaxis 45.0 10000

testSceneLights :: [Light]
testSceneLights = [ 
    QuadLight (CommonLightData (Colour 500 500 500 0) True) (Vector 0.0 200.0 (-300.0) 1.0) 1000 (Vector 1000.0 0.0 0.0 0.0) (Vector 0.0 0.0 1000.0 0.0)
    ]
