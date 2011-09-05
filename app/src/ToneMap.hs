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

import Colour

-- x = x
toneMapIdentity :: [Colour] -> [Colour]
toneMapIdentity = map id

-- TODO - Calculate averages and whatnot with a proper tail recursive method
-- x = x / avg xs
toneMapAverageLuminance :: [Colour] -> [Colour]
toneMapAverageLuminance xs = map (* invAverageBrightness) xs
    where
      colourSum = foldr (+) colBlack xs
      numColours = (fromIntegral . length) xs
      (Colour avgR avgG avgB _) = colourSum </> numColours
      invAverageBrightness = Colour (1 / avgR) (1 / avgG) (1 / avgB) 1

-- Reinhard tone map operator http://filmicgames.com/archives/75
toneMapReinhard :: [Colour] -> [Colour]
toneMapReinhard = map (\(Colour !r !g !b _) -> Colour (r / (r + 1)) (g / (g + 1)) (b / (b + 1)) 1)

-- Hejl-Burgess-Dawson http://filmicgames.com/archives/75
toneMapHejlBurgessDawson :: [Colour] -> [Colour]
toneMapHejlBurgessDawson = map f
    where
      f colour = (x * ((x <*> 6.2) <+> 0.5)) / (x * ((x <*> 6.2) <+> 1.7) <+> 0.06)
          where
            x = (\x' -> fold max x' 0) (colour <-> 0.004)

-- Apply a tone map operator
toneMapImage :: ([Colour] -> [Colour]) -> [Colour] -> [Colour]
toneMapImage f = f

-- Normal averaging
imageAverageLuminance :: [Colour] -> Double
imageAverageLuminance = imageAverageLuminance' 0 0
    where
      imageAverageLuminance' accLum accCount (x:xs) = imageAverageLuminance' (accLum + luminance x) (accCount + 1) xs
      imageAverageLuminance' accLum 0 [] = accLum
      imageAverageLuminance' accLum accCount [] = accLum / accCount

-- Get the average luminance of a scene, using Reinhard style log-lum averaging to damp down the effect of outlier pixels
imageAverageLogLuminance :: [Colour] -> Double
imageAverageLogLuminance = imageAverageLogLuminance' 0 0
    where
      imageAverageLogLuminance' accLum accCount (x:xs) = imageAverageLogLuminance' (accLum + logLuminance x) (accCount + 1) xs
      imageAverageLogLuminance' accLum 0 [] = accLum
      imageAverageLogLuminance' accLum accCount [] = exp (accLum / accCount)

-- Adjust the exposure of an image
exposeImage :: ([Colour] -> Double) -> [Colour] -> [Colour]
exposeImage f xs = map (</> (exposure * 2)) xs
    where
      exposure = f xs
