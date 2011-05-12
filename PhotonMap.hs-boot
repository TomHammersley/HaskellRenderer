-- HS-boot interface file for PhotonMap to break circular dependencies

module PhotonMap(buildPhotonMap, PhotonMap(photonList), irradiance, PhotonMapContext(PhotonMapContext)) where

import Colour
import Vector
import SceneGraph
import Material
import {-# SOURCE #-} Light

data PhotonMapContext = PhotonMapContext {
      photonGatherDistance :: Float,
      maxGatherPhotons :: Int,
      coneFilterK :: Float }

data Photon = Photon { power :: !Colour,
                       position :: !Position,
                       direction :: !Direction }

data PhotonMapTree = PhotonMapNode { splitAxis :: Int, splitValue :: Float, child0 :: PhotonMapTree, child1 :: PhotonMapTree} 
                   | PhotonMapLeaf { photon :: Photon }

data PhotonMap = PhotonMap { photonList :: [Photon],
                             photonMapTree :: PhotonMapTree }

irradiance :: PhotonMap -> (Position, TangentSpace) -> PhotonMapContext -> Material -> Colour
buildPhotonMap :: SceneGraph -> [Light] -> Int -> PhotonMap
