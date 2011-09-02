module Light (applyLight, surfaceEpsilon, Light(PointLight, AmbientLight, QuadLight), LightingResult, position, colour, range, deltaU, deltaV, addToPhotonMap) where

import Vector
import Colour
import Material
import SceneGraph

data Light = PointLight { position :: !Position, colour :: !Colour, range :: !Double, addToPhotonMap :: Bool }
           | AmbientLight { colour :: !Colour, addToPhotonMap :: Bool }
           | QuadLight { position :: !Position, deltaU :: !Direction, deltaV :: !Direction, colour :: !Colour, addToPhotonMap :: Bool }

type LightingResult = (Colour, Colour, Colour) -- Ambient, diffuse, specular

applyLight :: SceneGraph -> (Position, TangentSpace) -> Material -> Direction -> Light -> Colour
surfaceEpsilon :: Double
