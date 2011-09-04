-- Main module of raytracer

import Data.Bits
import Data.ByteString hiding (map)
import System.Console.GetOpt
import System.Environment
import RayTrace
import Colour
import SceneGraph
import KDTree
import CornellBox
import GHC.Conc (numCapabilities)
import Codec.BMP
import PhotonMap
import RenderContext
import Light
import ToneMap

data Option
    = ShowIntermediate -- -i
    | PhotonMap -- -p
      deriving (Eq, Ord, Enum, Show, Bounded)

options :: [OptDescr Option]
options = [
    Option ['i'] [] (NoArg ShowIntermediate) "Show intermediates",
    Option ['p'] [] (NoArg PhotonMap) "Photon map"
    ]

parsedOptions :: [String] -> [Option]
parsedOptions argv = case getOpt Permute options argv of
        (args,_,[]) -> args
        (_,_,_) -> []

-- Some hardcoded values, at present
renderWidth :: Int -> Int
renderWidth mipLevel = 1280 `shiftR` mipLevel

renderHeight :: Int -> Int
renderHeight mipLevel = 720 `shiftR` mipLevel

--renderSettings :: RenderContext

-- This returns a list of colours of pixels
renderImage :: Int -> RenderContext -> Maybe PhotonMap -> [Colour]
renderImage mipLevel renderSettings photonMap = finalImage
    where
      rawImageOutput = rayTraceImage renderSettings cornellBoxCamera (renderWidth mipLevel) (renderHeight mipLevel) photonMap
      toneMappedImage = toneMapImage toneMapReinhard rawImageOutput
      finalImage = map (clamp . invGammaCorrect) toneMappedImage

-- In the interest of rapid developer feedback, this functions writes a progressively-increasing image
-- So, we get quick feedback on the intermediate results, but will still ultimately get the final image
-- Note this does no re-use, so it'll be slower overall
writeRaytracedImage :: [Int] -> Maybe PhotonMap -> RenderContext -> IO ()
writeRaytracedImage [] photonMap renderSettings = do
  let imageData = renderImage 0 renderSettings photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth 0) (renderHeight 0) rgba
  writeBMP "test.bmp" bmp
writeRaytracedImage (mipLevel:mipLevels) photonMap renderSettings = do
  let imageData = renderImage mipLevel renderSettings photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth mipLevel) (renderHeight mipLevel) rgba
  let filename = "test-intermediate-" ++ show mipLevel ++ ".bmp"
  Prelude.putStrLn filename
  writeBMP filename bmp
  writeRaytracedImage mipLevels photonMap renderSettings

-- Strip off the photon map flag from a light
notInPhotonMap :: Light -> Light
notInPhotonMap (PointLight (CommonLightData colour' _) position' range') = PointLight (CommonLightData colour' False) position' range'
notInPhotonMap (AmbientLight (CommonLightData colour' _)) = AmbientLight (CommonLightData colour' False)
notInPhotonMap (QuadLight (CommonLightData colour' _) position' deltaU' deltaV') = QuadLight (CommonLightData colour' False) position' deltaU' deltaV'

-- Main function
main :: IO ()
main = do 
  args <- getArgs
  let opts = parsedOptions args

  let renderSettings = RenderContext 
                       numDistributedSamples 
                       (buildSceneGraph cornellBox generateSceneGraphUsingKDTree) 
                       cornellBoxLights 
                       maxRayDepth 
                       reflectionDistance 
                       refractionDistance 
                       (PhotonMapContext photonGatherDistance maxGatherPhotons coneFilterConstant) 
                       rayOriginDistribution'
                       depthOfFieldFocalDistance'
                       renderMode'
       where
         -- Ray trace constants
         numDistributedSamples = 64
         maxRayDepth = 5
         reflectionDistance = 1000
         refractionDistance = 1000
         -- Photon constants
         photonGatherDistance = 100
         maxGatherPhotons = 200
         coneFilterConstant = 2
         -- Depth of field constants
         rayOriginDistribution' = 0.5
         depthOfFieldFocalDistance' = 400
         renderMode'
             | PhotonMap `Prelude.elem` opts = PhotonMapper
             | otherwise = RayTrace

  Prelude.putStrLn $ "Running on " ++ show numCapabilities ++ " cores"
  let thousand = 1000
  let numPhotons = 100 * thousand
  let (photonMap, lights')
          | PhotonMap `Prelude.elem` opts = (\(a, b) -> (Just a, b)) $ buildPhotonMap (sceneGraph renderSettings) cornellBoxLights numPhotons
          | otherwise = (Nothing, map notInPhotonMap (lights renderSettings))
  let renderSettings' = renderSettings { lights = lights' }
  let maxMipLevel = 8
  let intermediateMipLevels = if ShowIntermediate `Prelude.elem` opts
                              then Prelude.reverse [1..maxMipLevel]
                              else []
  writeRaytracedImage intermediateMipLevels photonMap renderSettings'
