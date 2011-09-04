-- Tone map an image

module ToneMap(toneMapImage, toneMapIdentity, toneMapAverageLuminance, toneMapReinhard) where

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

-- Reinhard tone map operator
toneMapReinhard :: [Colour] -> [Colour]
toneMapReinhard = map (\(Colour r g b a) -> Colour (r / (r + 1)) (g / (g + 1)) (b / (b + 1)) 1)

-- Apply a tone map operator
toneMapImage :: ([Colour] -> [Colour]) -> [Colour] -> [Colour]
toneMapImage f xs = f xs
