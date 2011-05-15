-- Generic shaders to return colour and texture information

module Shader where

import Vector
import Colour
import Misc

data Shader = CheckedShader { scale :: {-# UNPACK #-} !Vector, colour1 :: {-# UNPACK #-} !Colour, colour2 :: {-# UNPACK #-} !Colour }
            | ShowNormalShader
            | NullShader deriving (Show, Read, Eq)

-- Functions available for each shader
evaluateAmbient :: Shader -> Position -> TangentSpace -> Colour
evaluateDiffuse :: Shader -> Position -> TangentSpace -> Colour
evaluateSpecular :: Shader -> Position -> TangentSpace -> Colour
shadePoint :: Shader -> (Position, Direction) -> (Colour, Colour, Colour) -> Colour

-- Checked shaders
evaluateDiffuse (CheckedShader checkScale checkColour1 checkColour2) position _ = 
    let scaledPosition = checkScale * position
        scaledX = round (vecX scaledPosition) :: Int
        scaledY = round (vecY scaledPosition) :: Int
        scaledZ = round (vecZ scaledPosition) :: Int
    in if odd scaledX `xor` odd scaledY `xor` odd scaledZ then checkColour1 else checkColour2

-- Normal display
evaluateDiffuse ShowNormalShader _ (_, _, normal) = encodeNormal normal

-- Null shader
evaluateDiffuse NullShader _ _ = colWhite

-- Defaults
evaluateSpecular = evaluateDiffuse
evaluateAmbient = evaluateDiffuse

-- New style shader interface
shadePoint (CheckedShader checkScale checkColour1 checkColour2) (position, _) (ambient, diffuse, specular) = (ambient + diffuse + specular) * checkColour
    where
      scaledPosition = checkScale * position
      scaledX = round (vecX scaledPosition) :: Int
      scaledY = round (vecY scaledPosition) :: Int
      scaledZ = round (vecZ scaledPosition) :: Int
      checkColour = if odd scaledX `xor` odd scaledY `xor` odd scaledZ then checkColour1 else checkColour2

shadePoint ShowNormalShader (_, Vector x y z _) (_, _, _) = Colour (x * 0.5 + 0.5) (y * 0.5 + 0.5) (z * 0.5 + 0.5) 1

-- Default fallback
shadePoint _ (_, _) (ambient, diffuse, specular) = ambient + diffuse + specular
