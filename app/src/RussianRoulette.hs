-- Shared module for russian roulette across path tracer and photon mapper

module RussianRoulette where

import Material
import Colour

-- Compute russian roulette coefficients
russianRouletteCoefficients :: Material -> (Double, Double)
russianRouletteCoefficients mat = (diffuseP, specularP)
    where
      (Colour diffuseR diffuseG diffuseB _) = Material.diffuse mat
      (Colour specularR specularG specularB _) = Material.specular mat
      diffuseP = (diffuseR + diffuseG + diffuseB) / 3
      specularP = (specularR + specularG + specularB) / 3
