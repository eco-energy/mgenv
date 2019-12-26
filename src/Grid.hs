{-# LANGUAGE DeriveAnyClass #-}
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}


module Grid (builder, GridState, SampledGrid, sampleGridSpec, generateGrid, initWorldTime, initGridState, gridStep) where

import Control.Monad.Bayes.Class
import Control.Monad (replicateM)

import GHC.Generics (Generic)


import Physics.Units (GeoC, R, Meters, EuclideanC, Theta, Watts, location, reverseHaversine, ZonedTime, incrementTime, unZonedTime, dateStartToUTC, fromUTC)



import Data.Time (addDays, diffDays, UTCTime, utcToZonedTime, NominalDiffTime, addUTCTime, zonedTimeToUTC, fromGregorian)
import HH (sampleHH, HHSpec, gridLoc, NodeId, HHState, hhStep)
import Physics.Transmission
import qualified Data.List.NonEmpty as NE

import Geometry.EMST (minSpanTreeEdges, positiveGridPoints)
import Algebra.Graph.Labelled
import qualified Data.Map as Map

import Elminator
import Data.Proxy

import Streamly
import qualified Streamly.Prelude as S

import qualified Elminator as E
import Data.Bifunctor

{-
Semantically, what is the purpose of the graph here?

An overlay of edges makes a grid, the consequences of which are
- parallel streams
- fold
The cost function is
sum batteryState 



--}


type Reward = R

newtype Grid e n = Grid
  { unGrid :: Graph e n
  } deriving (Eq, Show, Generic, Functor, Applicative, E.ToHType)

--type StateGrid = Grid Edge Node

type ControlGrid = Grid Bool Watts


mkGrid :: (Semigroup n, Monoid e) => Graph e n -> Grid e n
mkGrid = Grid

type StateGrid = Grid TransmissionState HHState

initWorldTime :: (MonadAsync m) => ZonedTime -> SerialT m ZonedTime 
initWorldTime initTime = S.iterate tn initTime
  where
  tn = incrementTime (1 :: NominalDiffTime)



gridStep :: StateGrid -> ControlGrid -> StateGrid
gridStep grid control = undefined
{--
bimap et ht grid
  where
    et = undefined
    ht = hhStep 
--}

gridReward :: StateGrid -> Reward
gridReward g = undefined
  
data GridSpec = GridSpec
  { startTime :: ZonedTime
  , centerPoint :: GeoC
  , nNodes :: Int
  , nodeDistanceMean :: R
  , nodeDistanceStd :: R
  } deriving (Eq, Show, Generic)


newtype SampledGrid = SampledGrid (Graph TransmissionSpec HHSpec) deriving (Eq, Show, Generic, E.ToHType)

newtype GridState = GridState (Graph TransmissionState HHState) deriving (Show, Generic, E.ToHType)

initGridState :: GridSpec -> GridState
initGridState gs = undefined

data Node = Node
  { node :: NodeId
  , coords :: EuclideanC
  , geoCoords :: GeoC
  } deriving (Eq, Show, Generic)

instance Ord Node where
  (Node n1 _ _) `compare` (Node n2 _ _) = n1 `compare` n2


mkSampledGrid :: Graph TransmissionSpec HHSpec -> SampledGrid
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


toHH :: (MonadSample m) => Node -> m HHSpec
toHH (Node{node, geoCoords, coords}) = sampleHH node geoCoords coords


type NodeDict = Map.Map NodeId Node

type Edge = (NodeId, NodeId)

generateGrid :: (MonadSample m) => GridSpec -> m (SampledGrid)
generateGrid GridSpec {..} = do
  nodeDistances <- replicateM nNodes $ normal nodeDistanceMean nodeDistanceStd
  nodeAngles <- replicateM nNodes $ uniform 0 360
  let
    (nodeDict, edges) = localAndGlobalLoc centerPoint nodeDistances nodeAngles
    edgeLoc :: Edge -> (Node, Node)
    edgeLoc (x, y) = ((nodeDict Map.! x), (nodeDict Map.! y))
  graphs <- mapM (uncurry sampleEdge) $ map edgeLoc edges
  return $ mkSampledGrid $ overlays graphs


localAndGlobalLoc :: GeoC -> [Meters] -> [Theta] -> (NodeDict, [Edge])
localAndGlobalLoc centerPoint nodeDistances nodeAngles = (nodeDict, edges)
  where
    geoCs = map (uncurry (atDistanceAndAngle centerPoint)) $ zip nodeDistances nodeAngles
    enumCs = zip [(0::NodeId)..] $ positiveGridPoints $ zip nodeDistances nodeAngles
    edges :: [(NodeId, NodeId)]
    edges = minSpanTreeEdges $ NE.fromList enumCs
    nodes :: [Node]
    nodes = map (\(g, (i,c))-> Node i c g) $ zip geoCs enumCs
    nodeDict = Map.fromList $ zip (map node nodes) nodes
    -- get the gps coordinate of a point a distance and at an angle away from another
    atDistanceAndAngle :: GeoC -> Meters -> Theta -> GeoC
    atDistanceAndAngle = reverseHaversine


sampleEdge :: (MonadSample m) => Node -> Node -> m (Graph TransmissionSpec HHSpec)
sampleEdge loc1 loc2 = do
  let
    distance = distanceMeters (coords loc1) (coords loc2)
    distanceMeters (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2
  tspec <- sampleTransmissionSpec distance
  h1 <- toHH loc1
  h2 <- toHH loc2
  return $ edge tspec h1 h2



builder :: Builder
builder = do
  include (Proxy :: Proxy GridState) $ Everything Mono
  include (Proxy :: Proxy SampledGrid) $ Everything Mono
