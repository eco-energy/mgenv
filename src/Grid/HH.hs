{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module HH
  ( sampleHH
  , HHSpec(..)
  , NodeId
  , HHState(..)
  , hhStep
  , initHHState
  ) where

import GHC.Generics (Generic)

import Control.Monad.State

import Control.Monad.Bayes.Class

-- 
import Physics.Storage
  (initBatteryState,  sampleBatterySpec
  , BatteryState
  , BatterySpec
  , batteryVoltage
  , stateNext
  , energyStored
  )

import Physics.PV
  ( samplePVSpec
  , runPV
  , PVSpec
  )
  

import Physics.Units
  ( unZonedTime
  , R
  , GeoC
  , EuclideanC
  , Watts
  , Amp
  , DelT
  , MetersPerSecond
  , Temperature
  , ZonedTime
  , unZonedTime
  )

import Physics.Consumption
  ( ConsumptionSpec
  , ConsumptionState
  , sampleConsumptionSpec
  , initConsumptionState
  , runConsumption
  )

-- A household tracks three types of State:
-- (Storage, Consumption, Transmission)
-- This state results in a Reward at the end of an episode
type NodeId = Int

type Reward = R

newtype HHState = HHState (BatteryState, ConsumptionState) deriving (Eq, Ord, Show, Generic)


initHHState :: ConsumptionSpec -> (BatteryState, ConsumptionState)
initHHState cs = (initBatteryState, initConsumptionState cs)

data HHSpec = HHSpec
  { nId :: NodeId
  , loc :: GeoC
  , gridLoc  :: EuclideanC
  , storage  :: BatterySpec
  , generation :: PVSpec
  , consumption :: ConsumptionSpec
  } deriving (Eq, Show, Generic)

instance Ord HHSpec where
  a `compare` b = (nId a) `compare` (nId b)


sampleHH :: (MonadSample m) => NodeId -> GeoC -> EuclideanC -> m HHSpec
sampleHH n loc grloc = do
  storage <- sampleBatterySpec
  gen <- samplePVSpec
  consump <- sampleConsumptionSpec
  return $ HHSpec n loc grloc storage gen consump


data Transmission = Transmission Watts Amp


hhStep' :: (MonadSample m) => HHSpec -> DelT -> ZonedTime -> MetersPerSecond -> Temperature -> Transmission -> StateT HHState m Reward
hhStep' HHSpec {..} delT time windSpeed ambientTemp transmission = do
  (HHState (bs, cs)) <- get
  (cs', consumed) <- runConsumption cs
  let
    (Transmission _ tc) = transmission
    generated = runPV loc generation (unZonedTime time) ambientTemp windSpeed
    bV = batteryVoltage bs
    generationCurrent = generated / bV
    consumptionCurrent = consumed / bV
    batteryCurrent = tc + generationCurrent + consumptionCurrent
    bs' = stateNext storage bs batteryCurrent delT 
    reward = (energyStored bs') + consumed
  put (HHState (bs', cs'))
  return reward

hhStep :: (MonadSample m) => HHSpec -> (DelT -> ZonedTime -> MetersPerSecond -> Temperature -> Transmission -> StateT HHState m Reward)
hhStep hspec = hhStep' hspec
