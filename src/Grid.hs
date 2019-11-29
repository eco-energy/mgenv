{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Grid where

import Streamly
import qualified Streamly.Prelude as S
import Control.Monad.State
import Control.Monad.Bayes.Class
import Algebra.Graph.Labelled

import GHC.Generics (Generic)

import Geodetics.Geodetic

import Physics.Units (GeoC, R, Meters, location, MetersPerSecond, Temperature, haversine, reverseHaversine)
import HH (HHSpec, sampleHH)
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
    cp = location 24.54743000 67.62771000
  nNodes <- uniformD [10..100]
  nodeDistanceMean <- uniform 10 50
  nodeDistanceStd <- uniform 10 20
  return $ GridSpec cp nNodes nodeDistanceMean nodeDistanceStd

type Grid = Graph HHSpec TransmissionSpec


mkGrid :: (MonadSample m) => GridSpec -> m Grid
mkGrid GridSpec {..} = do
  nodeDistances <- replicateM nNodes $ normal nodeDistanceMean nodeDistanceStd
  nodeAngles <- replicateM nNodes $ uniform 0 360
  let
    geoCs = map (uncurry (atDistanceAndAngle centerPoint)) $ zip nodeDistances nodeAngles 
    gridLocs = gridWithCenterAndPoints centerPoint $ zip nodeDistances nodeAngles 
  nodes <- mapM (uncurry sampleHH) $ zip geoCs gridLocs
  return Grid


gridWithCenterAndPoints :: GeoC -> [(Meters, Theta)] -> [(Meters, Meters)]
gridWithCenterAndPoints = undefined

atDistanceAndAngle :: GeoC -> Meters -> Theta -> GeoC
atDistanceAndAngle = reverseHaversine



-- This should be at grid level
data EnvCond = EnvCond
  { windSpeed :: MetersPerSecond
  , ambientTemp :: Temperature
  } deriving (Eq, Ord, Show, Generic)

{--
setupDay :: GeoC -> ZonedTime -> (ZonedTime, ZonedTime)
setupDay loc day = (start, end)
  where
    (start, end) = sunRiseAndSet loc verticalShift lcd
    lcd = zonedTimeToLCD day
    verticalShift = 0.833333
--}

sampleEnvCond :: (MonadSample m) => m EnvCond
sampleEnvCond = do
  windSpeed <- liftM abs $ normal 1 5
  ambientTemp <- normal 20 10
  return $ EnvCond windSpeed ambientTemp
--}
