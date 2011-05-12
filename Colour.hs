{-# LANGUAGE BangPatterns #-}

module Colour where
import Vector hiding (min, max)
import Misc
import Data.Word

-- Normalised RGBA colour
data Colour = Colour { red :: {-# UNPACK #-} !Float, 
                       green :: {-# UNPACK #-} !Float, 
                       blue :: {-# UNPACK #-} !Float, 
                       alpha :: {-# UNPACK #-} !Float } deriving (Show, Read, Ord, Eq)

instance Num Colour where
    {-# SPECIALIZE INLINE (+) :: Colour -> Colour -> Colour #-}
    (Colour !r1 !g1 !b1 !a1) + (Colour !r2 !g2 !b2 !a2) = Colour (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
    {-# SPECIALIZE INLINE (-) :: Colour -> Colour -> Colour #-}
    (Colour !r1 !g1 !b1 !a1) - (Colour !r2 !g2 !b2 !a2) = Colour (r1 - r2) (g1 - g2) (b1 - b2) (a1 - a2)
    {-# SPECIALIZE INLINE (*) :: Colour -> Colour -> Colour #-}
    (Colour !r1 !g1 !b1 !a1) * (Colour !r2 !g2 !b2 !a2) = Colour (r1 * r2) (g1 * g2) (b1 * b2) (a1 * a2)
    abs (Colour r g b a) = Colour (abs r) (abs g) (abs b) (abs a)
    signum (Colour r g b a) = Colour (signum r) (signum g) (signum b) (signum a)
    fromInteger x = Colour (fromInteger x) (fromInteger x) (fromInteger x) (fromInteger x)

-- Multiply a colour by a scalar
colourMul :: Colour -> Float -> Colour
(Colour !r !g !b !a) `colourMul` m = Colour (r * m) (g * m) (b * m) (a * m)

(<*>) :: Colour -> Float -> Colour
(Colour !r !g !b !a) <*> k = Colour (r * k) (g * k) (b * k) (a * k)

(</>) :: Colour -> Float -> Colour
(Colour !r !g !b !a) </> k = Colour (r / k) (g / k) (b / k) (a / k)

clamp :: Colour -> Colour
clamp (Colour !r !g !b !a) = Colour (max 0 (min r 1)) (max 0 (min g 1)) (max 0 (min b 1)) (max 0 (min a 1))

-- Basic colours
colRed :: Colour
colRed = Colour 1 0 0 1

colGreen :: Colour
colGreen = Colour 0 1 0 1

colBlue :: Colour
colBlue = Colour 0 0 1 1

colWhite :: Colour
colWhite = Colour 1 1 1 1

colBlack :: Colour
colBlack = Colour 0 0 0 1

colGrey :: Colour
colGrey = Colour 0.5 0.5 0.5 1

colYellow :: Colour
colYellow = Colour 1 1 0 1

gamma :: Float
gamma = 2.2

invGamma :: Float
invGamma = 1.0 / gamma

-- Gamma correct a colour
gammaCorrect :: Colour -> Colour
gammaCorrect (Colour !r !g !b !a) = Colour (r ** gamma) (g ** gamma) (b ** gamma) (a ** gamma)

invGammaCorrect ::Colour -> Colour
invGammaCorrect (Colour !r !g !b !a) = Colour (r ** invGamma) (g ** invGamma) (b ** invGamma) (a ** invGamma)

-- Colour encode a normal
encodeNormal :: Vector -> Colour
encodeNormal (Vector x y z _) = gammaCorrect $ Colour (saturate $ x * 0.5 + 0.5) (saturate $ y * 0.5 + 0.5) (saturate $ z * 0.5 + 0.5) 1

-- Convert a list of colours to a list of Word8s
convertColoursToPixels :: [Colour] -> [Word8]
convertColoursToPixels (col:cols) = r : g : b : 255 : (convertColoursToPixels cols)
    where
      r = truncate ((red col) * 255.0)
      g = truncate ((green col) * 255.0)
      b = truncate ((blue col) * 255.0)
convertColoursToPixels [] = []

-- Measure overall magnitude of a colour
magnitude :: Colour -> Float
magnitude (Colour r g b _) = r * 0.3 + g * 0.6 + b * 0.1

-- Convert to a list
toListRGBA :: Colour -> [Float]
toListRGBA (Colour r g b a) = [r, g, b, a]

toListRGB :: Colour -> [Float]
toListRGB (Colour r g b _) = [r, g, b]
