-- Tone map an image

module ToneMap(toneMapImage, 
               toneMapIdentity, 
               toneMapAverageLuminance, 
               toneMapReinhard, 
               toneMapHejlBurgessDawson) where

import Colour

-- x = x
toneMapIdentity :: [Colour] -> [Colour]
toneMapIdentity = map (\x -> x)

-- TODO - Calculate averages and whatnot with a proper tail recursive method
-- x = x / avg xs
toneMapAverageLuminance :: [Colour] -> [Colour]
toneMapAverageLuminance xs = map (\x -> x * invAverageBrightness) xs
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
toneMapImage f xs = f xs
