{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Grid where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.State
import Control.Monad.Bayes.Class
import Algebra.Graph.Labelled

import GHC.Generics (Generic)

import Physics.Units (GeoC, R, Meters)
import HH
import Physics.Transmission

type Theta = R
type Gamma = R
 
type GridPoints = (R, Theta, Gamma)

data GridSpec = GridSpec
  { centerPoint :: GeoC
  , nNodes :: Int
  , nodeDistanceMean :: R
  , nodeDistanceStd :: R
  } deriving (Eq, Show, Generic)

sampleGridSpec :: MonadSample m => m GridSpec
sampleGridSpec = do
  let
    cp = (24.54743, 67.62771) :: GeoC
  nNodes <- uniformD [10..100]
  nodeDistanceMean <- uniform 10 50
  nodeDistanceStd <- uniform 10 20
  return $ GridSpec cp nNodes nodeDistanceMean nodeDistanceStd

type Grid = Graph HHSpec TransmissionSpec


mkGrid :: (MonadSample m) => GridSpec -> Grid
mkGrid GridSpec {..} = do
  nodeDistances <- replicateM nNodes $ normal nodeDistanceMean nodeDistanceStd
  nodeAngles <- replicateM nNodes $ uniform 0 360
  let
    geoCs = map $ atDistanceAndAngle cp $ zip nodeDistances nodeAngles 
    gridLocs = gridWithCenterAndPoints cp $ zip nodeDistances nodeAngles 
  nodes <- mapM sampleHH nodeDistances
  return Grid


gridWithCenterAndPoints :: GeoC -> [(Meters, Theta)] -> [(Meters, Meters)]
gridWithCenterAndPoints = undefined

atDistanceAndAngle :: GeoC -> Meters -> Theta -> GeoC
atDistanceAndAngle = undefined
