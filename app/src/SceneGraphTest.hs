module SceneGraphTest where

import SceneGraph
import Primitive
import Material
import Matrix
import Shader
import Colour

testObjs1 = [
    Object (Sphere 50) (Material colRed colRed colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 0 100)
    ]

testObjs2 = [
    Object (Sphere 50) (Material colRed colRed colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 0 100),
    Object (Sphere 50) (Material colGreen colGreen colWhite 50 0.5 0 iorAir NullShader) (translationMatrix 200 0 100)
    ]

testObjs3 = [
    Object (Sphere 50) (Material colRed colRed colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 0 100),
    Object (Sphere 50) (Material colGreen colGreen colWhite 50 0.5 0 iorAir NullShader) (translationMatrix 200 0 100),
    Object (Sphere 50) (Material colBlue colBlue colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 200 100)
    ]

testObjs4 = [
    Object (Sphere 50) (Material colRed colRed colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 0 100),
    Object (Sphere 50) (Material colGreen colGreen colWhite 50 0.5 0 iorAir NullShader) (translationMatrix 200 0 100),
    Object (Sphere 50) (Material colBlue colBlue colWhite 50 0.5 0 iorAir NullShader) (translationMatrix (-200) 200 100),
    Object (Sphere 50) (Material colYellow colYellow colWhite 50 0.5 0 iorAir NullShader) (translationMatrix 200 200 100)
    ]
