module Light (applyLight, surfaceEpsilon, Light(PointLight, AmbientLight, QuadLight), LightingResult, position, colour, range, deltaU, deltaV) where

import Vector
import Colour
import Material
import SceneGraph

data Light = PointLight { position :: !Position, colour :: !Colour, range :: !Float, addToPhotonMap :: Bool }
           | AmbientLight { colour :: !Colour }
           | QuadLight { position :: !Position, deltaU :: !Direction, deltaV :: !Direction, colour :: !Colour }

type LightingResult = (Colour, Colour, Colour) -- Ambient, diffuse, specular

applyLight :: SceneGraph -> (Position, TangentSpace) -> Material -> Direction -> Light -> Colour
surfaceEpsilon :: Float
