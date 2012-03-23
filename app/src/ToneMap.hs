-- Tone map an image
{-# LANGUAGE BangPatterns #-}

module ToneMap(toneMapImage, 
               toneMapIdentity, 
               toneMapAverageLuminance, 
               toneMapReinhard, 
               toneMapHejlBurgessDawson,
               exposeImage,
               imageAverageLogLuminance,
               imageAverageLuminance) where

import PolymorphicNum
import Colour
import Data.List

-- x = x
toneMapIdentity :: [Colour] -> [Colour]
toneMapIdentity = map id

-- x = x / avg xs
toneMapAverageLuminance :: [Colour] -> [Colour]
toneMapAverageLuminance xs = map (<*> invAverageBrightness) xs
    where
      invAverageBrightness = 1 / imageAverageLuminance xs

-- Reinhard tone map operator http://filmicgames.com/archives/75
toneMapReinhard :: [Colour] -> [Colour]
toneMapReinhard = map (\(Colour r g b _) -> Colour (r / (r + 1)) (g / (g + 1)) (b / (b + 1)) 1)

-- Hejl-Burgess-Dawson http://filmicgames.com/archives/75
toneMapHejlBurgessDawson :: [Colour] -> [Colour]
toneMapHejlBurgessDawson = map f
    where
      f colour = (x <*> (x <*> (6.2 :: Double) <+> (0.5 :: Double))) </> (x <*> (x <*> (6.2 :: Double) <+> (1.7 :: Double)) <+> (0.06:: Double))
          where
            x = (\x' -> fold max x' 0) (colour <-> (0.004 :: Double))

-- Apply a tone map operator
toneMapImage :: ([Colour] -> [Colour]) -> [Colour] -> [Colour]
toneMapImage f = f

-- Normal averaging
imageAverageLuminance :: [Colour] -> Double
imageAverageLuminance xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0 :: Integer) xs
    step (!s', !l') a = (s' + luminance a, l' + 1)

-- Get the average luminance of a scene, using Reinhard style log-lum averaging to damp down the effect of outlier pixels
imageAverageLogLuminance :: [Colour] -> Double
imageAverageLogLuminance xs = exp (s / fromIntegral l)
  where
    (s, l) = foldl' step (0, 0 :: Integer) xs
    step (!s', !l') a = (s' + logLuminance a, l' + 1)

-- Adjust the exposure of an image
exposeImage :: ([Colour] -> Double) -> [Colour] -> Double -> [Colour]
exposeImage f xs exposureScale = map (</> (exposure * exposureScale)) xs
    where
      exposure = f xs
