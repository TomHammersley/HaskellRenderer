{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Colour where
import Vector hiding (min, max)
import Misc
import Data.Word
import Control.DeepSeq
import PolymorphicNum

-- Normalised RGBA colour
data Colour = Colour { red :: {-# UNPACK #-} !Double, 
                       green :: {-# UNPACK #-} !Double, 
                       blue :: {-# UNPACK #-} !Double, 
                       alpha :: {-# UNPACK #-} !Double } deriving (Show, Read, Ord, Eq)

instance NFData Colour where
    rnf (Colour r g b a) = rnf r `seq` rnf g `seq` rnf b `seq` rnf a

instance PolymorphicNum Colour Colour Colour where
    (Colour !r !g !b !a) <*> (Colour !r' !g' !b' !a') = Colour (r * r') (g * g') (b * b') (a * a')
    (Colour !r !g !b !a) </> (Colour !r' !g' !b' !a') = Colour (r / r') (g / g') (b / b') (a / a')
    (Colour !r !g !b !a) <-> (Colour !r' !g' !b' !a') = Colour (r - r') (g - g') (b - b') (a - a')
    (Colour !r !g !b !a) <+> (Colour !r' !g' !b' !a') = Colour (r + r') (g + g') (b + b') (a + a')

instance PolymorphicNum Colour Double Colour where
    (Colour !r !g !b !a) <*> k = Colour (r * k) (g * k) (b * k) (a * k)
    (Colour !r !g !b !a) </> k = Colour (r / k) (g / k) (b / k) (a / k)
    (Colour !r !g !b !a) <-> k = Colour (r - k) (g - k) (b - k) (a - k)
    (Colour !r !g !b !a) <+> k = Colour (r + k) (g + k) (b + k) (a + k)

instance PolymorphicNum Double Colour Colour where
    k <*> (Colour !r !g !b !a) = Colour (k * r) (k * g) (k * b) (k * a)
    k </> (Colour !r !g !b !a) = Colour (k / r) (k / g) (k / b) (k / a)
    k <-> (Colour !r !g !b !a) = Colour (k - r) (k - g) (k - b) (k - a)
    k <+> (Colour !r !g !b !a) = Colour (k + r) (k + g) (k + b) (k + a)

clamp :: Colour -> Colour
clamp (Colour !r !g !b !a) = Colour (max 0 (min r 1)) (max 0 (min g 1)) (max 0 (min b 1)) (max 0 (min a 1))

fold :: (Double -> Double -> Double) -> Colour -> Double -> Colour
fold f (Colour !r !g !b !a) k = Colour (f r k) (f g k) (f b k) (f a k)

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

colZero :: Colour
colZero = Colour 0 0 0 0

colGrey :: Colour
colGrey = Colour 0.5 0.5 0.5 1

colYellow :: Colour
colYellow = Colour 1 1 0 1

gamma :: Double
gamma = 2.2

invGamma :: Double
invGamma = 1.0 / gamma

-- Gamma correct a colour
gammaCorrect :: Colour -> Colour
gammaCorrect (Colour !r !g !b !a) = Colour (r ** gamma) (g ** gamma) (b ** gamma) (a ** gamma)

invGammaCorrect ::Colour -> Colour
invGammaCorrect (Colour !r !g !b !a) = Colour (r ** invGamma) (g ** invGamma) (b ** invGamma) (a ** invGamma)

-- Colour encode a normal
encodeNormal :: Vector -> Colour
encodeNormal (Vector !x !y !z _) = gammaCorrect $ Colour (saturate $ x * 0.5 + 0.5) (saturate $ y * 0.5 + 0.5) (saturate $ z * 0.5 + 0.5) 1

-- Convert a list of colours to a list of Word8s
convertColoursToPixels :: [Colour] -> [Word8]
convertColoursToPixels (col:cols) = r : g : b : 255 : convertColoursToPixels cols
    where
      r = truncate (red col * 255.0)
      g = truncate (green col * 255.0)
      b = truncate (blue col * 255.0)
convertColoursToPixels [] = []

-- Measure overall magnitude of a colour
magnitude :: Colour -> Double
magnitude (Colour r g b _) = r * 0.3 + g * 0.6 + b * 0.1

-- Convert to a list
toListRGBA :: Colour -> [Double]
toListRGBA (Colour r g b a) = [r, g, b, a]

toListRGB :: Colour -> [Double]
toListRGB (Colour r g b _) = [r, g, b]

luminance :: Colour -> Double
luminance (Colour !r !g !b _) = r * 0.3 + g * 0.6 + b * 0.1

logLuminance :: Colour -> Double
logLuminance = log . max 1e-5 . luminance
