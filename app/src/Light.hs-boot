module Light (applyLight, 
       	      surfaceEpsilon, 
	      Light(PointLight, AmbientLight, QuadLight), 
	      CommonLightData(CommonLightData), 
	      LightingResult, 
	      position, 
	      colour, 
	      range, 
	      deltaU, 
	      deltaV, 
	      addToPhotonMap, 
	      common) where

import Vector
import Colour
import Material
import SceneGraph

data CommonLightData = CommonLightData { colour :: !Colour,
                                         addToPhotonMap :: !Bool } 

data Light = PointLight { common :: CommonLightData, position :: !Position, range :: !Double }
           | AmbientLight { common :: CommonLightData }
           | QuadLight { common :: CommonLightData, position :: !Position, range :: !Double, deltaU :: !Direction, deltaV :: !Direction } 

type LightingResult = (Colour, Colour, Colour) -- Ambient, diffuse, specular

applyLight :: SceneGraph -> (Position, TangentSpace) -> Material -> Direction -> Light -> Colour
surfaceEpsilon :: Double
