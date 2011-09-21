-- Cornell box reference data
{-# LANGUAGE MagicHash #-}

module CornellBox(cornellBox, cornellBoxCamera, cornellBoxLights) where

import Vector
import Primitive
import Camera
import Material
import Colour
import Shader
import Matrix
import Light
--import GHC.Types

cornellBoxLights :: [Light]

cornellBoxCamera :: Camera

cameraPosition :: Vector

floorObject :: Object
leftWallObject :: Object
rightWallObject :: Object
frontWallObject :: Object
ceilingObject :: Object
backWallObject :: Object
tallBlockObject :: Object
shortBlockObject :: Object
lightObject :: Object

leftWallVertices :: [Vector]
rightWallVertices :: [Vector]
backWallVertices :: [Vector]
frontWallVertices :: [Vector]
ceilingVertices :: [Vector]
lightVertices :: [Vector]
floorVertices :: [Vector]
tallBlockVertices :: [Vector]
shortBlockVertices :: [Vector]

whiteMaterial :: Material
redMaterial :: Material
greenMaterial :: Material
lightMaterial :: Material

cornellBoxLights = [ 
    QuadLight (CommonLightData (Colour 500 500 500 0) True) (Vector 213.0 548.0 227.0 1.0) 600 (Vector 130.0 0.0 0.0 0.0) (Vector 0.0 0.0 105.0 0.0)
    ]

cameraPosition = Vector 278.0 273.0 (-2000.0) 1.0
cornellBoxCamera = withVectors cameraPosition xaxis yaxis zaxis 45.0 10000

whiteMaterial = Material (Colour 0.5 0.5 0.5 1) (Colour 0.5 0.5 0.5 1) colBlack colBlack 0 0 0 iorAir NullShader
redMaterial   = Material (Colour 0.5 0.0 0.0 1) (Colour 0.5 0.0 0.0 1) colBlack colBlack 0 0 0 iorAir NullShader
greenMaterial = Material (Colour 0.0 0.5 0.0 1) (Colour 0.0 0.5 0.0 1) colBlack colBlack 0 0 0 iorAir NullShader
lightMaterial = Material colBlack colBlack colBlack (Colour 1000 1000 1000 1) 0 0 0 iorAir NullShader

lightVertices = [
    Vector 343.0 548.0 227.0 1.0,
    Vector 343.0 548.0 342.2 1.0,
    Vector 213.0 548.0 342.0 1.0,
    Vector 213.0 548.0 227.2 1.0
    ]

floorVertices = [
    Vector 556.0 0.0   0.0 1.0,
    Vector   0.0 0.0   0.0 1.0,
    Vector   0.0 0.0 559.2 1.0,
    Vector 556.0 0.0 559.2 1.0
    ]

ceilingVertices = [
    Vector 556.0 548.8   0.0 1.0,
    Vector 556.0 548.8 559.2 1.0,
    Vector   0.0 548.8 559.2 1.0,
    Vector   0.0 548.8   0.0 1.0
    ]

backWallVertices = [
    Vector 556.0   0.0 559.2 1.0,
    Vector   0.0   0.0 559.2 1.0,
    Vector   0.0 548.8 559.2 1.0,
    Vector 556.0 548.8 559.2 1.0
    ]

frontWallVertices = [
    Vector 556.0 548.8 0.0 1.0,
    Vector   0.0 548.8 0.0 1.0,
    Vector   0.0   0.0 0.0 1.0,
    Vector 556.0   0.0 0.0 1.0
    ]

rightWallVertices = [
    Vector 0.0   0.0 559.2 1.0,
    Vector 0.0   0.0   0.0 1.0,
    Vector 0.0 548.8   0.0 1.0,
    Vector 0.0 548.8 559.2 1.0
    ]

leftWallVertices = [
    Vector 556.0   0.0   0.0 1.0,
    Vector 556.0   0.0 559.2 1.0,
    Vector 556.0 548.8 559.2 1.0,
    Vector 556.0 548.8   0.0 1.0
    ]

shortBlockVertices = [
    Vector 130.0 165.0  65.0 1.0,
    Vector  82.0 165.0 225.0 1.0,
    Vector 240.0 165.0 272.0 1.0,
    Vector 290.0 165.0 114.0 1.0,

    Vector 290.0   0.0 114.0 1.0,
    Vector 290.0 165.0 114.0 1.0,
    Vector 240.0 165.0 272.0 1.0,
    Vector 240.0   0.0 272.0 1.0,

    Vector 130.0   0.0  65.0 1.0,
    Vector 130.0 165.0  65.0 1.0,
    Vector 290.0 165.0 114.0 1.0,
    Vector 290.0   0.0 114.0 1.0,

    Vector  82.0   0.0 225.0 1.0,
    Vector  82.0 165.0 225.0 1.0,
    Vector 130.0 165.0  65.0 1.0,
    Vector 130.0   0.0  65.0 1.0,

    Vector 240.0   0.0 272.0 1.0,
    Vector 240.0 165.0 272.0 1.0,
    Vector  82.0 165.0 225.0 1.0,
    Vector  82.0   0.0 225.0 1.0
    ]

tallBlockVertices = [
    Vector 423.0 330.0 247.0 1.0,
    Vector 265.0 330.0 296.0 1.0,
    Vector 314.0 330.0 456.0 1.0,
    Vector 472.0 330.0 406.0 1.0,

    Vector 423.0   0.0 247.0 1.0,
    Vector 423.0 330.0 247.0 1.0,
    Vector 472.0 330.0 406.0 1.0,
    Vector 472.0   0.0 406.0 1.0,

    Vector 472.0   0.0 406.0 1.0,
    Vector 472.0 330.0 406.0 1.0,
    Vector 314.0 330.0 456.0 1.0,
    Vector 314.0   0.0 456.0 1.0,

    Vector 314.0   0.0 456.0 1.0,
    Vector 314.0 330.0 456.0 1.0,
    Vector 265.0 330.0 296.0 1.0,
    Vector 265.0   0.0 296.0 1.0,

    Vector 265.0   0.0 296.0 1.0,
    Vector 265.0 330.0 296.0 1.0,
    Vector 423.0 330.0 247.0 1.0,
    Vector 423.0   0.0 247.0 1.0
    ]

floorObject = Object (TriangleMesh (quadsToTriangles floorVertices)) whiteMaterial identity
frontWallObject = Object (TriangleMesh (quadsToTriangles frontWallVertices)) whiteMaterial identity
leftWallObject = Object (TriangleMesh (quadsToTriangles leftWallVertices)) redMaterial identity
rightWallObject = Object (TriangleMesh (quadsToTriangles rightWallVertices)) greenMaterial identity
ceilingObject = Object (TriangleMesh (quadsToTriangles ceilingVertices)) whiteMaterial identity
backWallObject = Object (TriangleMesh (quadsToTriangles backWallVertices)) whiteMaterial identity
shortBlockObject = Object (TriangleMesh (quadsToTriangles shortBlockVertices)) whiteMaterial identity
tallBlockObject = Object (TriangleMesh (quadsToTriangles tallBlockVertices)) whiteMaterial identity
lightObject = Object (TriangleMesh (quadsToTriangles lightVertices)) lightMaterial identity

cornellBox :: [Object]
cornellBox = [ceilingObject, floorObject, leftWallObject, rightWallObject, backWallObject, frontWallObject, tallBlockObject, shortBlockObject, lightObject]
