-- Tone map an image

module ToneMap(toneMapImage, toneMapIdentity) where

import Colour

toneMapIdentity :: [Colour] -> [Colour]
toneMapIdentity = map (\x -> x)

toneMapImage :: ([Colour] -> [Colour]) -> [Colour] -> [Colour]
toneMapImage f xs = f xs
