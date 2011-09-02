-- Materials of an object

module Material where

import Colour
import Shader

data Material = Material { ambient :: {-# UNPACK #-} !Colour, 
                           diffuse :: {-# UNPACK #-} !Colour, 
                           specular :: {-# UNPACK #-} !Colour, 
                           specularPower :: {-# UNPACK #-} !Double,
                           reflectivity :: {-# UNPACK #-} !Double,
                           transmit :: {-# UNPACK #-} !Double,
                           indexOfRefraction :: {-# UNPACK #-} !Double,
                           shader :: Shader } deriving (Show, Eq)

iorAir :: Double
iorAir = 1.000293

iorWater :: Double
iorWater = 1.3330
