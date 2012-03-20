-- Photon mapping

module PhotonMap(buildPhotonMap, PhotonMap(photonList), irradiance, PhotonMapContext(PhotonMapContext)) where

--import PolymorphicNum
import {-# SOURCE #-} Light hiding (position)
import Vector
--import Distribution
import Material
import Colour
import SceneGraph
--import Ray hiding (direction)
--import Control.Monad.State
--import BoundingBox
--import KDTree
--import Debug.Trace
--import Misc
--import Control.Parallel.Strategies
--import Control.DeepSeq
--import Data.Heap hiding (partition)
--import System.Random
--import Data.List hiding (union, insert)
--import Primitive
--import RussianRoulette

data PhotonMapContext = PhotonMapContext {
      photonGatherDistance :: Double,
      maxGatherPhotons :: Int,
      coneFilterK :: Double,
      directVisualisation :: Bool }

data Photon = Photon { power :: {-# UNPACK #-} !Colour, posDir :: {-# UNPACK #-} !(Position, Direction) }

data PhotonMapTree = PhotonMapNode {-# UNPACK #-} !Int {-# UNPACK #-} !Double PhotonMapTree PhotonMapTree
                   | PhotonMapLeaf {-# UNPACK #-} !Photon

data PhotonMap = PhotonMap { photonList :: [Photon],
                             photonMapTree :: PhotonMapTree }

buildPhotonMap :: SceneGraph -> [Light] -> Int -> (PhotonMap, [Light])
irradiance :: PhotonMap -> PhotonMapContext -> Material -> SurfaceLocation -> (Colour, Double)
