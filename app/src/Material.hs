-- Materials of an object

module Material where

import Colour
import Shader

data Material = Material { ambient :: {-# UNPACK #-} !Colour, 
                           diffuse :: {-# UNPACK #-} !Colour, 
                           specular :: {-# UNPACK #-} !Colour, 
                           emission :: {-# UNPACK #-} !Colour, 
                           specularPower :: {-# UNPACK #-} !Double,
                           reflectivity :: {-# UNPACK #-} !Double,
                           transmit :: {-# UNPACK #-} !Double,
                           indexOfRefraction :: {-# UNPACK #-} !Double,
                           shader :: Shader } deriving (Show, Eq)

iorAir :: Double
iorAir = 1.000293

iorWater :: Double
iorWater = 1.3330

defaultMaterial :: Material
defaultMaterial = Material 
                  (Colour 0.5 0.5 0.5 0.5)
                  (Colour 0.5 0.5 0.5 0.5)
                  (Colour 0.5 0.5 0.5 0.5)
                  (Colour 0.0 0.0 0.0 0.0)
                  25
                  0
                  0
                  iorAir
                  NullShader
                  