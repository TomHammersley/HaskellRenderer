-- Shared module for russian roulette across path tracer and photon mapper

module RussianRoulette where

import Material
import Colour

data RussianRouletteChoice = DiffuseReflect | SpecularReflect | Absorb deriving (Show, Eq)

-- Compute russian roulette coefficients
russianRouletteCoefficients :: Material -> (Double, Double)
russianRouletteCoefficients mat = (diffuseP, specularP)
    where
      diffuseP = (magnitude . Material.diffuse) mat
      specularP = (magnitude . Material.specular) mat

russianRouletteCoefficients2 :: Material -> (Double, Double)
russianRouletteCoefficients2 mat = (diffuseP, 1 - diffuseP)
  where
    (Colour dr dg db _) = diffuse mat
    (Colour sr sg sb _) = specular mat
    diffuseP = (dr + dg + db) / (dr + dg + db + sr + sg + sb)
    
