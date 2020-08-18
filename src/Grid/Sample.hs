{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Grid.Sample
  ( GridState(..)
  , SampledGrid(..)
  , GridSpec(..)
  , defGridSpec
  , sampleGridSpec
  , generateGrid
  , initWorldTime
  , initGridState
  , gridStep
  ) where

import Control.Monad.Bayes.Class
import Control.Monad (replicateM)

import GHC.Generics (Generic)


import Physics.Units
  ( GeoC
  , R
  , Meters
  , EuclideanC
  , BearingDeg
  , Watts
  , ZonedTime
  , location
  , reverseHaversine
  , incrementTime
  , dateStartToUTC
  , fromUTC
  )


import RL.MDP
import Prob.Randomizable


import Data.Time (NominalDiffTime, fromGregorian)
import Grid.HH (initHHState, HHSpec(..), sampleHH, NodeId, HHState(..), hhStep)
import Physics.Transmission
import qualified Data.List.NonEmpty as NE

import Geometry.EMST (minSpanTreeEdges, positiveGridPoints)
import qualified Data.Map as Map

import Data.Proxy

import Streamly
import qualified Streamly.Prelude as S

import Data.Bifunctor
import Data.Monoid
import qualified Algebra.Graph as G
import qualified Algebra.Graph.Labelled as LG
import Algebra.Graph.Class

import Control.Monad.State

{-
Semantically, what is the purpose of the graph here?

An overlay of edges makes a grid, the consequences of which are
- parallel streams
- fold
The cost function is
sum batteryState 
--}


newtype Grid e n = Grid
  { unGrid :: LG.Graph e n
  } deriving (Eq, Show, Generic, Functor)



mkGrid :: (Semigroup n, Monoid e) => LG.Graph e n -> Grid e n
mkGrid = Grid

type StateGrid = Grid TransmissionState HHState

initWorldTime :: (MonadAsync m) => ZonedTime -> SerialT m ZonedTime 
initWorldTime initTime = S.iterate tn initTime
  where
  tn = incrementTime (1 :: NominalDiffTime)



gridStep grid control = undefined

  
data GridSpec = GridSpec
  { startTime :: ZonedTime
  , centerPoint :: GeoC
  , nNodes :: Int
  , nodeDistanceMean :: R
  , nodeDistanceStd :: R
  } deriving (Eq, Show, Generic)

instance Randomizable GridSpec where
  sampleThis = sampleGridSpec


newtype SampledGrid = SampledGrid (LG.Graph TransmissionSpec HHSpec) deriving (Eq, Show, Generic)

newtype GridState = GridState (LG.Graph TransmissionState HHState) deriving (Show, Generic)


data Node = Node
  { node :: NodeId
  , coords :: EuclideanC
  , geoCoords :: GeoC
  } deriving (Eq, Show, Generic)

instance Ord Node where
  (Node n1 _ _) `compare` (Node n2 _ _) = n1 `compare` n2


mkSampledGrid :: LG.Graph TransmissionSpec HHSpec -> SampledGrid
mkSampledGrid = SampledGrid

sampleGridSpec :: MonadSample m => m GridSpec
sampleGridSpec = do
  let
    cp = location 24.54743000 67.62771000
    startDate = (fromGregorian  2015 1 1)
    endDate = (fromGregorian 2024 12 12)
  nNodes <- uniformD [10..100]
  nodeDistanceMean <- uniform 10 50
  nodeDistanceStd <- uniform 10 20
  t' <- uniformD (enumFromTo startDate endDate)
  let st = fromUTC $ dateStartToUTC t'
  return $ GridSpec st cp nNodes nodeDistanceMean nodeDistanceStd

defGridSpec :: GridSpec
defGridSpec = GridSpec
  (fromUTC $ dateStartToUTC $ fromGregorian 2020 05 01)
  (location 24.54743000 67.62771000)
  3
  10
  5

initGridState :: SampledGrid -> GridState
initGridState (SampledGrid gs) = GridState $ bimap txInit (\HHSpec{..} -> HHState $ initHHState consumption) gs
  where
    txInit tx
      | tx == mempty = mempty
      | otherwise = TransmissionState 0.001

toHH :: (MonadSample m) => Node -> m HHSpec
toHH (Node{node, geoCoords, coords}) = sampleHH node geoCoords coords


type NodeDict = Map.Map NodeId Node

type Edge = (NodeId, NodeId)

generateGrid :: (MonadSample m) => GridSpec -> m (SampledGrid)
generateGrid GridSpec {..} = do
  nodeDistances <- replicateM nNodes $ normal nodeDistanceMean nodeDistanceStd
  nodeAngles <- replicateM nNodes $ uniform 0 360
  let
    (nodeDict, tedges) = localAndGlobalLoc centerPoint nodeDistances nodeAngles
    edgeLoc :: Edge -> (Node, Node)
    edgeLoc (x, y) = ((nodeDict Map.! x), (nodeDict Map.! y))
  gEdges <- mapM (uncurry sampleEdge) $ map edgeLoc tedges
  return $ mkSampledGrid $ LG.edges gEdges


localAndGlobalLoc :: GeoC -> [Meters] -> [BearingDeg] -> (NodeDict, [Edge])
localAndGlobalLoc centerPoint nodeDistances nodeAngles = (nodeDict, tedges)
  where
    geoCs = map (uncurry (atDistanceAndAngle centerPoint)) $ zip nodeDistances nodeAngles
    enumCs = zip [(0::NodeId)..] $ positiveGridPoints $ zip nodeDistances nodeAngles
    tedges :: [(NodeId, NodeId)]
    tedges = minSpanTreeEdges $ NE.fromList enumCs
    nodes :: [Node]
    nodes = map (\(g, (i,c))-> Node i c g) $ zip geoCs enumCs
    nodeDict = Map.fromList $ zip (map node nodes) nodes
    -- get the gps coordinate of a point a distance and at an angle away from another
    atDistanceAndAngle :: GeoC -> Meters -> BearingDeg -> GeoC
    atDistanceAndAngle = reverseHaversine


sampleEdge :: (MonadSample m) => Node -> Node -> m (TransmissionSpec, HHSpec, HHSpec)
sampleEdge loc1 loc2 = do
  let
    distance = distanceMeters (coords loc1) (coords loc2)
    distanceMeters (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2
  tspec <- sampleTransmissionSpec --distance
  h1 <- toHH loc1
  h2 <- toHH loc2
  return $ (tspec, h1, h2)




-- a smooth, real-valued function H over a symplectic manifold defines a hamiltonian system.
-- The symplectic manifold is the phase space P.
-- The vector field induced by H over P is hamiltonian vector field which induces a
-- time-parameter family of transformations of P, in an isotopy of symplectomorphisms, begining at identity.
-- Symplectomorphisms preserve the volume form on the phase space.
hamiltonian f = undefined


