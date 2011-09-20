-- HS-boot interface file for PhotonMap to break circular dependencies

module PhotonMap(buildPhotonMap, PhotonMap(photonList), irradiance, PhotonMapContext(PhotonMapContext)) where

import Colour
import Vector
import SceneGraph
import Material
import {-# SOURCE #-} Light

data PhotonMapContext = PhotonMapContext {
      photonGatherDistance :: Double,
      maxGatherPhotons :: Int,
      coneFilterK :: Double,
      directVisualisation :: Bool }

data Photon = Photon { power :: !Colour,
                       position :: !Position,
                       direction :: !Direction }

data PhotonMapTree = PhotonMapNode { splitAxis :: Int, splitValue :: Double, child0 :: PhotonMapTree, child1 :: PhotonMapTree} 
                   | PhotonMapLeaf { photon :: Photon }

data PhotonMap = PhotonMap { photonList :: [Photon],
                             photonMapTree :: PhotonMapTree }

irradiance :: PhotonMap -> PhotonMapContext -> Material -> (Position, TangentSpace) -> (Colour, Double)
buildPhotonMap :: SceneGraph -> [Light] -> Int -> (PhotonMap, [Light])
