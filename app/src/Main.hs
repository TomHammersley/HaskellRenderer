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
import Control.Arrow

-- Command line option support
data Option
    = ShowIntermediate -- -i
    | PhotonMap -- -p
    | DirectPhotonMapVisualisation -- -v
    | DistributedRayTracing -- d
    | IrradianceCaching -- c
    | PathTrace -- P
      deriving (Eq, Ord, Enum, Show, Bounded)

options :: [OptDescr Option]
options = [
    Option "i" [] (NoArg ShowIntermediate) "Show intermediates",
    Option "p" [] (NoArg PhotonMap) "Photon map",
    Option "v" [] (NoArg DirectPhotonMapVisualisation) "Direct photon map visualisation",
    Option "d" [] (NoArg DistributedRayTracing) "Distributed ray tracing",
    Option "c" [] (NoArg IrradianceCaching) "Irradiance caching",
    Option "P" [] (NoArg PathTrace) "Path tracing"
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

-- This returns a list of colours of pixels
renderScaledImage :: Int -> RenderContext -> Maybe PhotonMap -> [Colour]
renderScaledImage mipLevel renderSettings photonMap = finalImage
    where
      rawImageOutput = renderScene photonMap renderSettings cornellBoxCamera (renderWidth mipLevel) (renderHeight mipLevel) 
      exposedImage = exposeImage imageAverageLuminance rawImageOutput 4
      toneMappedImage = toneMapImage toneMapHejlBurgessDawson exposedImage
      finalImage = map (clamp . invGammaCorrect) toneMappedImage

-- In the interest of rapid developer feedback, this functions writes a progressively-increasing image
-- So, we get quick feedback on the intermediate results, but will still ultimately get the final image
-- Note this does no re-use, so it'll be slower overall
writeImageMipMapChain :: String -> [Int] -> Maybe PhotonMap -> RenderContext -> IO ()
writeImageMipMapChain baseFilename [] photonMap renderSettings = do
  let imageData = renderScaledImage 0 renderSettings photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth 0) (renderHeight 0) rgba
  Prelude.putStrLn "Performing final render"
  writeBMP (baseFilename ++ ".bmp") bmp
writeImageMipMapChain baseFilename (mipLevel:mipLevels) photonMap renderSettings = do
  let imageData = renderScaledImage mipLevel renderSettings photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth mipLevel) (renderHeight mipLevel) rgba
  let filename = baseFilename ++ "-" ++ show mipLevel ++ ".bmp"
  Prelude.putStrLn filename
  writeBMP filename bmp
  writeImageMipMapChain baseFilename mipLevels photonMap renderSettings

-- Strip off the photon map flag from a light
notInPhotonMap :: Light -> Light
notInPhotonMap (PointLight (CommonLightData colour' _) position' range') = PointLight (CommonLightData colour' False) position' range'
notInPhotonMap (AmbientLight (CommonLightData colour' _)) = AmbientLight (CommonLightData colour' False)
notInPhotonMap (QuadLight (CommonLightData colour' _) position' range' deltaU' deltaV') = QuadLight (CommonLightData colour' False) position' range' deltaU' deltaV'

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
                       (PhotonMapContext photonGatherDistance maxGatherPhotons coneFilterConstant directPhotonMapVisualisation) 
                       rayOriginDistribution'
                       depthOfFieldFocalDistance'
                       renderMode'
                       enableIrradianceCache
       where
         -- Ray trace constants
         numDistributedSamples = if DistributedRayTracing `Prelude.elem` opts 
                                 then 64 
                                 else 1
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
             | PathTrace `Prelude.elem` opts = PathTracer
             | otherwise = RayTrace
         directPhotonMapVisualisation = DirectPhotonMapVisualisation `Prelude.elem` opts
         enableIrradianceCache = IrradianceCaching `Prelude.elem` opts

  -- Display hardware capabilities
  Prelude.putStrLn $ "Running on " ++ show numCapabilities ++ " cores"

  -- Create a photon map, if necessary
  let doPhotonMapping = PhotonMap `Prelude.elem` opts
  let photonMapMessage = if doPhotonMapping 
                         then if DirectPhotonMapVisualisation `Prelude.elem` opts
                              then "Directly visualising photon map"
                              else "Creating photon map..." 
                         else "Photon mapping disabled"
  Prelude.putStrLn photonMapMessage
  let thousand = 1000
  let numPhotons = 200 * thousand
  let (photonMap, lights')
          | doPhotonMapping = Control.Arrow.first Just $ 
                              buildPhotonMap (sceneGraph renderSettings) cornellBoxLights numPhotons
          | otherwise = (Nothing, map notInPhotonMap (lights renderSettings))

  -- Display message about irradiance cache
  Prelude.putStrLn (if useIrradianceCache renderSettings then "Irradiance caching enabled" else "Irrradiance caching disabled")

  -- Display message about path tracing
  Prelude.putStrLn (if PathTrace `Prelude.elem` opts then "Path tracer enabled" else "Path tracer disabled")

  -- Render the image
  let renderSettings' = renderSettings { lights = lights' }
  let maxMipLevel = 8
  let intermediateMipLevels = if ShowIntermediate `Prelude.elem` opts
                              then Prelude.reverse [1..maxMipLevel]
                              else []
  Prelude.putStrLn "Rendering image..."
  writeImageMipMapChain "render-output" intermediateMipLevels photonMap renderSettings'
