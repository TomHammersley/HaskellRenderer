-- Main module of raytracer

import RayTrace
import Colour
import SceneGraph
import KDTree
import CornellBox
import GHC.Conc (numCapabilities)
import Codec.BMP
import Data.ByteString
--import System.Console.GetOpt
import PhotonMap
import Data.Bits

-- Some hardcoded values, at present
renderWidth :: Int -> Int
renderWidth mipLevel = 1280 `shiftR` mipLevel

renderHeight :: Int -> Int
renderHeight mipLevel = 720 `shiftR` mipLevel

sceneGraph :: SceneGraph
sceneGraph = buildSceneGraph cornellBox generateSceneGraphUsingKDTree

renderSettings :: RenderContext
renderSettings = RenderContext numDistributedSamples sceneGraph cornellBoxLights maxRayDepth reflectionDistance refractionDistance (PhotonMapContext photonGatherDistance maxGatherPhotons coneFilterConstant) rayOriginDistribution depthOfFieldFocalDistance
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
      rayOriginDistribution = 0.5
      depthOfFieldFocalDistance = 400

-- This returns a list of colours of pixels
raytracedImage :: Int -> PhotonMap -> [Colour]
raytracedImage mipLevel = rayTraceImage renderSettings cornellBoxCamera (renderWidth mipLevel) (renderHeight mipLevel)

-- In the interest of rapid developer feedback, this functions writes a progressively-increasing image
-- So, we get quick feedback on the intermediate results, but will still ultimately get the final image
-- Note this does no re-use, so it'll be slower overall
writeRaytracedImage :: [Int] -> PhotonMap -> IO ()
writeRaytracedImage [] photonMap = do
  let imageData = raytracedImage 0 photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth 0) (renderHeight 0) rgba
  writeBMP "test.bmp" bmp
writeRaytracedImage (mipLevel:mipLevels) photonMap = do
  let imageData = raytracedImage mipLevel photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP (renderWidth mipLevel) (renderHeight mipLevel) rgba
  let filename = "test-intermediate-" ++ show mipLevel ++ ".bmp"
  Prelude.putStrLn filename
  writeBMP filename bmp
  writeRaytracedImage mipLevels photonMap

-- Main function
main :: IO ()
main = do 
  Prelude.putStrLn $ "Running on " ++ show numCapabilities ++ " cores"
  let thousand = 1000
  let numPhotons = 100 * thousand
  let photonMap = buildPhotonMap sceneGraph cornellBoxLights numPhotons
  let maxMipLevel = 8
--  writeRaytracedImage [7] photonMap
  writeRaytracedImage (Prelude.reverse [1..maxMipLevel]) photonMap
