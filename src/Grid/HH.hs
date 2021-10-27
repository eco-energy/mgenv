{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Grid.HH
  ( sampleHH
  , HHSpec(..)
  , NodeId
  , HHState(..)
  , hhS
  , runHH
  , initHHState
  ) where

import ConCat.Misc hiding (R, C)
import GHC.Generics hiding (R, C)

import Control.Monad.State

import Control.Monad.Bayes.Class

import Data.Void

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as P
import Streamly.Internal.Data.Pipe (Pipe(..))

import Physics.Storage
  (initBatteryState,  sampleBatterySpec
  , BatteryState
  , BatterySpec
  , batteryVoltage
  , batteryS
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
  , V
  , Amp
  , Ohm
  , fromUTC
  )

import Physics.Time (hence, henceUF, absToUTC, T, T', IntervalT)

import Physics.Consumption
  ( ConsumptionSpec
  , ConsumptionState
  , sampleConsumptionSpec
  , initConsumptionState
  , consumptionS
  )

import Physics.Transmission
  (TransmissionState(..), TransmissionSpec(..), Transmission(..), sendTx, recieveTx)

import Grid hiding (Transmission)
-- A household tracks three types of State:
-- (Storage, Consumption, Transmission)
-- This state results in a Reward at the end of an episode
type NodeId = Int

type Reward = R

-- battery :: VI s grid battery
--           -> VI s load battery
--           -> VI s gen battery
--           -> C s (battery s) (battery s)
--           -> C s ((grid :*: load :*: gen) s) (battery s)
-- battery (VI (gv, gi)) (lv, li) (gnv, gni) c = applyC c

data HH s grid battery load gen where
  Storage :: C s ((grid :*: load :*: gen) s) (battery s) -> HH s grid battery load gen


newtype HHState' m a = HHState' (Pipe m (HHSpec :* TransmissionSpec) (BatteryState, ConsumptionState, Watts, Transmission))
data HHState (m :: (* -> *)) a =
  HHState { batteryState :: Pipe m a BatteryState
          , consumptionState :: Pipe m a (ConsumptionState, Watts)
          , generationState :: Pipe m a Watts
          , txState :: Pipe m a (Watts, Watts)
          }
  deriving (Generic)


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


hhS :: (S.MonadAsync m, MonadSample m)
  => HHState m a
  -> Pipe m (T m, Wind m, T m) (BatteryState, (ConsumptionState, Watts), (Watts, Watts), Watts)
hhS HHState{..} = (,,,) <$> (a batteryState) <*> (a consumptionState) <*> (a txState) <*> (a generationState)
  where
    a = S.adapt

type Wind m = UF.Unfold m (GeoC, T') MetersPerSecond

type Temp m = UF.Unfold m (GeoC, T') Temperature


windy :: Wind m
windy = undefined

temperate :: Temp m
temperate = undefined

    
mkHH :: forall m a. (S.MonadAsync m, MonadSample m)
  => T m
  -> Wind m
  -> Temp m
  -> TransmissionSpec
  -> HHSpec
  -> HHState m a
mkHH HHSpec{..} delT time windSpeed ambientTemp transmission = HHState
  { batteryState = bs'
  , consumptionState = cs
  , txState = runTransmission tspec tx
  , generationState = generated
  }
  where
    t = hence 1000 time delT
    cs = consumptionS consumption
    consumed = fmap snd cs
    cstate = fmap fst cs
    (TransmissionState (tspec, tx@(Transmission vSrc iT vSink))) = transmission 
    pvT t = runPV loc generation (unZonedTime . toZonedTime $ t) ambientTemp windSpeed
    generated :: t m Watts
    generated = S.map pvT t
    bV :: t m BatteryState -> t m V
    bV = fmap batteryVoltage
    generationCurrent = S.zipWith (/) generated . bV
    consumptionCurrent = S.zipWith (/) consumed . bV
    txCurrent = iT
    batteryCurrent :: t m BatteryState -> t m Amp
    batteryCurrent s = zipSum (zipSum iT (generationCurrent s)) (consumptionCurrent s)
    bs' = batteryS storage (S.zipWith (\t' bc -> (snd t', bc)) t (batteryCurrent bs')) 
    reward = S.zipWith (+) (fmap energyStored bs') consumed
    zipSum = S.zipWith (+)

toZonedTime :: T' -> ZonedTime
toZonedTime = fromUTC . absToUTC . fst
-- hhStep :: forall t m. (S.IsStream t, S.MonadAsync m, MonadSample m) =>
--   HHSpec -> (DelT -> ZonedTime -> MetersPerSecond -> Temperature -> Transmission -> HHState t m)
-- hhStep hspec = hhStep' hspec
