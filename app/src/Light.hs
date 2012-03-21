-- Module for lights

module Light (applyLight, 
              surfaceEpsilon, 
              Light(PointLight, AmbientLight, QuadLight),
              CommonLightData(CommonLightData), 
              LightingResult,
              position, 
              colour, 
              range, 
              deltaU, 
              deltaV, 
              addToPhotonMap, 
              common) where

import PolymorphicNum
import Vector
import Colour
import Ray
import Material
import Shader
import SceneGraph
import Misc
import Control.Monad.State
import ShadowCache

data CommonLightData = CommonLightData { colour :: !Colour,
                                         addToPhotonMap :: !Bool } deriving (Show)

data Light = PointLight { common :: CommonLightData, position :: !Position, range :: !Double }
           | AmbientLight { common :: CommonLightData }
           | QuadLight { common :: CommonLightData, position :: !Position, range :: !Double, deltaU :: !Direction, deltaV :: !Direction } deriving (Show)

type LightingResult = (Colour, Colour, Colour) -- Ambient, diffuse, specular

-- Value for the surface epsilon
surfaceEpsilon :: Double
surfaceEpsilon = 0.1

-- Find the attenuation for a light source
lightAttenuation :: Vector -> Vector -> Double -> Double
lightAttenuation lightPos shadePos lightRange =
    let dist = lightPos `Vector.distance` shadePos
    in if dist < lightRange then 1 - dist / lightRange else 0

-- Apply phong lighting to an object
phongLighting :: SurfaceLocation -> Light -> Material -> SceneGraph -> Direction -> State ShadowCache Colour
phongLighting (shadePos, tanSpace) (PointLight (CommonLightData lightColour inPhotonMap') lightPos lightRange) objMaterial sceneGraph viewDirection 
    | (lightPos `distanceSq` shadePos) < (lightRange * lightRange) && dotProd > 0 = 
      do
        shadowCache <- get
        let (intersection, shadowCache') = testShadowCache shadowCache sceneGraph (rayWithPoints intersectionPlusEpsilon lightPos)
        let result = case intersection of
              Just _ -> colBlack -- An object is closer to our point of consideration than the light, so occluded
              Nothing -> (lightColour <*> lightingSum) <*> attenuation
                where
                  lightingSum = diffuseLighting <+> specularLighting
                  attenuation = lightAttenuation lightPos shadePos lightRange
                  specularCorrection = (specularPower objMaterial + 2) / (2 * pi)
                  specularLighting = specular objMaterial <*> (specularCorrection * saturate (reflection `dot3` Vector.negate viewDirection) ** specularPower objMaterial)
                  reflection = reflect incoming normal
                  diffuseLighting = if inPhotonMap'
                                    then colBlack
                                    else shaderDiffuse <*> diffuse objMaterial <*> saturate dotProd
                  shaderDiffuse = evaluateDiffuse (shader objMaterial) shadePos tanSpace
        put shadowCache'
        return result
    | otherwise = return colBlack
    where 
      intersectionPlusEpsilon = shadePos <+> normal <*> surfaceEpsilon
      incoming = normalise (lightPos <-> shadePos)
      dotProd = normal `dot3` incoming
      normal = thr tanSpace
phongLighting _ (AmbientLight (CommonLightData _ _)) _ _ _ = error "phongLighting: Do not know how to handle AmbientLight"
phongLighting (shadePos, tanSpace) (QuadLight (CommonLightData lightColour inPhotonMap') lightPos lightRange du dv) objMaterial sceneGraph viewDirection 
    | (lightCentre `distanceSq` shadePos) < (lightRange * lightRange) && dotProd > 0 = 
      do
        shadowCache <- get
        let (intersection, shadowCache') = testShadowCache shadowCache sceneGraph (rayWithPoints intersectionPlusEpsilon lightCentre)
        let result = case intersection of
              Just _ -> colBlack -- An object is closer to our point of consideration than the light, so occluded
              Nothing -> let lightingSum = diffuseLighting <+> specularLighting
                             attenuation = lightAttenuation lightCentre shadePos lightRange
                             specularCorrection = (specularPower objMaterial + 2) / (2 * pi)
                             specularLighting = specular objMaterial <*> (specularCorrection * saturate (reflection `dot3` Vector.negate viewDirection) ** specularPower objMaterial)
                             reflection = reflect incoming normal
                             
                             diffuseLighting = if inPhotonMap'
                                               then colBlack
                                               else shaderDiffuse <*> diffuse objMaterial <*> saturate dotProd
                             shaderDiffuse = evaluateDiffuse (shader objMaterial) shadePos tanSpace
                         in lightColour <*> lightingSum <*> attenuation
        put shadowCache'
        return result
    | otherwise = return colBlack
    where 
      lightCentre = lightPos <+> du <*> (0.5 :: Double) <+> dv <*> (0.5 :: Double)
      intersectionPlusEpsilon = shadePos <+> normal <*> surfaceEpsilon
      incoming = normalise (lightCentre <-> shadePos)
      dotProd = normal `dot3` incoming
      normal = thr tanSpace

-- For a given surface point, work out the lighting, including occlusion
applyLight :: SceneGraph -> SurfaceLocation -> Material -> Direction -> Light -> State ShadowCache Colour
applyLight sceneGraph intersectionPointNormal objMaterial viewDirection light@(PointLight (CommonLightData _ _) _ _)
    = phongLighting 
      intersectionPointNormal 
      light
      objMaterial 
      sceneGraph
      viewDirection
applyLight _ (intersectionPoint, intersectionTanSpace) objMaterial _ (AmbientLight (CommonLightData ambientColour _)) = 
    let shaderAmbient = evaluateAmbient (shader objMaterial) intersectionPoint intersectionTanSpace
        materialAmbient = ambient objMaterial
    in return $! ambientColour <*> shaderAmbient <*> materialAmbient
applyLight sceneGraph intersectionPointNormal objMaterial viewDirection light@(QuadLight (CommonLightData _ _) _ _ _ _)
    = phongLighting 
      intersectionPointNormal 
      light
      objMaterial 
      sceneGraph
      viewDirection
