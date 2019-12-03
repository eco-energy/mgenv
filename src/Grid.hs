{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Grid () where

import Control.Monad.Bayes.Class
import Control.Monad (replicateM)

import GHC.Generics (Generic)


import Physics.Units (GeoC, R, Meters, location, reverseHaversine, EuclideanC, Theta)
import HH (sampleHH, HHSpec, gridLoc, NodeId)
import Physics.Transmission
import qualified Data.List.NonEmpty as NE

import Geometry.EMST (minSpanTreeEdges, positiveGridPoints)
import Algebra.Graph.Labelled
import qualified Data.Map as Map

data GridSpec = GridSpec
  { centerPoint :: GeoC
  , nNodes :: Int
  , nodeDistanceMean :: R
  , nodeDistanceStd :: R
  } deriving (Eq, Show, Generic)

sampleGridSpec :: MonadSample m => m GridSpec
sampleGridSpec = do
  let
    cp = location 24.54743000 67.62771000
  nNodes <- uniformD [10..100]
  nodeDistanceMean <- uniform 10 50
  nodeDistanceStd <- uniform 10 20
  return $ GridSpec cp nNodes nodeDistanceMean nodeDistanceStd


type Grid = Graph HHSpec TransmissionSpec

data LocId = LocId
  { node :: NodeId
  , coords :: EuclideanC
  , geoCoords :: GeoC
  } deriving (Eq, Show, Generic)


instance Ord LocId where
  (LocId n1 _ _) `compare` (LocId n2 _ _) = n1 `compare` n2


mkGrid :: [HHSpec] -> [EdgeSpec] -> Grid
mkGrid = undefined

-- get the gps coordinate of a point a distance and at an angle away from another
atDistanceAndAngle :: GeoC -> Meters -> Theta -> GeoC
atDistanceAndAngle = reverseHaversine


generateGrid :: (MonadSample m) => GridSpec -> m (Graph HHSpec EdgeSpec)
generateGrid GridSpec {..} = do
  nodeDistances <- replicateM nNodes $ normal nodeDistanceMean nodeDistanceStd
  nodeAngles <- replicateM nNodes $ uniform 0 360
  let
    (locIds, edges) = localAndGlobalLoc centerPoint nodeDistances nodeAngles
    locDict = Map.fromList $ zip (map node locIds) locIds
    edgeLoc :: EdgeId -> (LocId, LocId)
    edgeLoc (x, y) = ((locDict Map.! x), (locDict Map.! y))
    toHH (LocId{node, geoCoords, coords}) = do
                   return $ vertex $ sampleHH node geoCoords coords
    toEdge (Loc)
  hhs <- mapM toHH locIds
  -- create nodes inside the edge map
  edgeSpecs <- mapM (uncurry sampleEdgeSpec) $ map edgeLoc edges
  mapM edges
  return $ map verte edgeSpecs

localAndGlobalLoc :: GeoC -> [Meters] -> [Theta] -> ([LocId], [EdgeId])
localAndGlobalLoc centerPoint nodeDistances nodeAngles = (gridLocs, edges)
  where
    geoCs = map (uncurry (atDistanceAndAngle centerPoint)) $ zip nodeDistances nodeAngles
    enumCs = zip [(0::NodeId)..] $ positiveGridPoints $ zip nodeDistances nodeAngles
    edges :: [EdgeId]
    edges = minSpanTreeEdges $ NE.fromList enumCs
    gridLocs :: [LocId]
    gridLocs = map (\(g, (i,c))-> LocId i c g) $ zip geoCs enumCs


type EdgeId = (NodeId, NodeId)


data EdgeSpec = EdgeSpec
  { transmissionSpec :: TransmissionSpec
  , distance :: Meters
  , n1 :: NodeId
  , n2 :: NodeId
  } deriving (Eq, Show, Generic)

sampleEdgeSpec :: (MonadSample m) => LocId -> LocId -> m EdgeSpec
sampleEdgeSpec (LocId{n1, c1, g1}) (LocId{n2, c2, g2}) = do
  let
    distance = distanceMeters (coords loc1) (coords loc2)
  tspec <- sampleTransmissionSpec distance
  hhspec <- sampleHHSpec 
  return $ EdgeSpec tspec distance (node loc1) (node loc2)

distanceMeters :: EuclideanC -> EuclideanC -> Meters
distanceMeters (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2 

  
--node (Node rootLabel subForest) = rootLabel

--parseSubforest :: Forest a -> [[a]]
--parseSubforest ts = map branches ts
