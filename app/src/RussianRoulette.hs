-- Shared module for russian roulette across path tracer and photon mapper

module RussianRoulette where

import Material
import Colour

data RussianRouletteChoice = DiffuseReflect | SpecularReflect | Absorb deriving Eq

-- Compute russian roulette coefficients
russianRouletteCoefficients :: Material -> (Double, Double)
russianRouletteCoefficients mat = (diffuseP, specularP)
    where
      diffuseP = (magnitude . Material.diffuse) mat
      specularP = (magnitude . Material.specular) mat

russianRouletteCoefficients2 :: Material -> (Double, Double)
russianRouletteCoefficients2 mat = (diffuseP, specularP)
    where
      diffuseP = (maxChannel . Material.diffuse) mat
      specularP = (maxChannel . Material.specular) mat
