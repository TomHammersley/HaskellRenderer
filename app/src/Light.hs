-- Module for lights
{-# LANGUAGE BangPatterns #-}

module Light (applyLight, surfaceEpsilon, Light(PointLight, AmbientLight, QuadLight), LightingResult, position, colour, range, deltaU, deltaV, addToPhotonMap) where

import Vector
import Colour
import Ray
import Material
import Shader
import SceneGraph
import Misc
import RayTrace (findAnyIntersection)

data Light = PointLight { position :: !Position, colour :: !Colour, range :: !Float, addToPhotonMap :: Bool }
           | AmbientLight { colour :: !Colour }
           | QuadLight { position :: !Position, deltaU :: !Direction, deltaV :: !Direction, colour :: !Colour }

type LightingResult = (Colour, Colour, Colour) -- Ambient, diffuse, specular

-- Value for the surface epsilon
surfaceEpsilon :: Float
surfaceEpsilon = 0.1

-- Find the attenuation for a light source
lightAttenuation :: Vector -> Vector -> Float -> Float
lightAttenuation !lightPos !shadePos !lightRange =
    let dist = lightPos `Vector.distance` shadePos
    in if dist < lightRange then 1 - dist / lightRange else 0

-- Apply phong lighting to an object
phongLighting :: SurfaceLocation -> Light -> Material -> SceneGraph -> Direction -> Colour
phongLighting (!shadePos, !tanSpace) (PointLight !lightPos !lightColour !lightRange inPhotonMap) objMaterial sceneGraph !viewDirection 
    | (lightPos `distanceSq` shadePos) < (lightRange * lightRange) && dotProd > 0 = case findAnyIntersection sceneGraph (rayWithPoints intersectionPlusEpsilon lightPos) of
                                                                                      Just _ -> colBlack -- An object is closer to our point of consideration than the light, so occluded
                                                                                      Nothing -> (lightColour * lightingSum) `colourMul` attenuation
                                                                                          where
                                                                                            lightingSum = diffuseLighting + specularLighting
                                                                                            attenuation = lightAttenuation lightPos shadePos lightRange
                                                                                            specularCorrection = (specularPower objMaterial + 2) / (2 * pi)
                                                                                            specularLighting = specular objMaterial `colourMul` (specularCorrection * saturate (reflection `dot3` Vector.negate viewDirection) ** specularPower objMaterial)
                                                                                            reflection = reflect incoming normal
                                                                                            diffuseLighting = if inPhotonMap
                                                                                                              then colBlack
                                                                                                              else shaderDiffuse * diffuse objMaterial `colourMul` saturate dotProd
                                                                                            shaderDiffuse = evaluateDiffuse (shader objMaterial) shadePos tanSpace
    | otherwise = colBlack
    where !intersectionPlusEpsilon = shadePos + (normal Vector.<*> surfaceEpsilon)
          !incoming = normalise (lightPos - shadePos)
          !dotProd = normal `dot3` incoming
          !normal = thr tanSpace
phongLighting _ (AmbientLight _) _ _ _ = error "phongLighting: Do not know how to handle AmbientLight"
-- TODO Implement specular calculations for this light. We assume it is always in the photon map
phongLighting _ (QuadLight _ _ _ _) _ _ _ = colBlack 

-- For a given surface point, work out the lighting, including occlusion
applyLight :: SceneGraph -> SurfaceLocation -> Material -> Direction -> Light -> Colour
applyLight sceneGraph !intersectionPointNormal !objMaterial !viewDirection (PointLight !lightPos !lightColour !lightRange !inPhotonMap) = phongLighting intersectionPointNormal (PointLight lightPos lightColour lightRange inPhotonMap) objMaterial sceneGraph viewDirection
applyLight _ (!intersectionPoint, !intersectionTanSpace) !objMaterial _ (AmbientLight !ambientColour) = 
    let shaderAmbient = evaluateAmbient (shader objMaterial) intersectionPoint intersectionTanSpace
        materialAmbient = ambient objMaterial
    in ambientColour * shaderAmbient * materialAmbient
applyLight _ (_, _) _ _ (QuadLight _ _ _ _) = colBlack
