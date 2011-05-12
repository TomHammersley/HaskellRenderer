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
renderWidth = 640

renderHeight :: Int
renderHeight = 480

sceneGraph :: SceneGraph
sceneGraph = buildSceneGraph cornellBox generateSceneGraphUsingKDTree

renderSettings :: RenderContext
renderSettings = RenderContext numDistributedSamples sceneGraph cornellBoxLights maxRayDepth reflectionDistance refractionDistance (PhotonMapContext photonGatherDistance maxGatherPhotons 2)
    where
      numDistributedSamples = 64
      maxRayDepth = 5
      photonGatherDistance = 50
      reflectionDistance = 1000
      refractionDistance = 1000
      maxGatherPhotons = 50 -- 500

-- This returns a list of colours of pixels
raytracedImage :: PhotonMap -> [Colour]
raytracedImage photonMap = rayTraceImage renderSettings cornellBoxCamera renderWidth renderHeight photonMap

-- Main function
main :: IO ()
main = do 
  Prelude.putStrLn $ "Running on " ++ show numCapabilities ++ " cores"
  photonMap <- return $ buildPhotonMap sceneGraph cornellBoxLights 10000 -- 50000
--  Prelude.putStrLn $ show photonMap
  Prelude.putStrLn $ "Num photons: " ++ show (Prelude.length (photonList photonMap))
  imageData <- return (raytracedImage photonMap)
  let rgba = Data.ByteString.pack (convertColoursToPixels imageData)
  let bmp = packRGBA32ToBMP renderWidth renderHeight rgba
  writeBMP "test.bmp" bmp
