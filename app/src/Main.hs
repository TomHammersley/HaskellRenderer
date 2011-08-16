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

-- Some hardcoded values, at present
renderWidth :: Int
renderWidth = 320 --1024

renderHeight :: Int
renderHeight = 200 --768

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
raytracedImage :: PhotonMap -> [Colour]
raytracedImage = rayTraceImage renderSettings cornellBoxCamera renderWidth renderHeight

-- Main function
main :: IO ()
main = do 
  Prelude.putStrLn $ "Running on " ++ show numCapabilities ++ " cores"
  let thousand = 1000
  let numPhotons = 100 * thousand
  let photonMap = buildPhotonMap sceneGraph cornellBoxLights numPhotons
--  Prelude.putStrLn $ "Num photons: " ++ show (Prelude.length (photonList photonMap))
  let imageData = raytracedImage photonMap
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP renderWidth renderHeight rgba
  writeBMP "test.bmp" bmp
