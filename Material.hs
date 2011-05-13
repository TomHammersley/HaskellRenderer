-- Materials of an object

module Material where

import Colour
import Shader

data Material = Material { ambient :: {-# UNPACK #-} !Colour, 
                           diffuse :: {-# UNPACK #-} !Colour, 
                           specular :: {-# UNPACK #-} !Colour, 
                           specularPower :: {-# UNPACK #-} !Float,
                           reflectivity :: {-# UNPACK #-} !Float,
                           transmit :: {-# UNPACK #-} !Float,
                           indexOfRefraction :: {-# UNPACK #-} !Float,
                           shader :: Shader } deriving (Show, Read, Eq)

iorAir :: Float
iorAir = 1.000293

iorWater :: Float
iorWater = 1.3330
