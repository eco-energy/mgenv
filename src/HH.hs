{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module HH where

import Data.Time (ZonedTime)
import Streamly
import Streamly.Prelude as S

import GHC.Generics (Generic)

import Control.Monad.State

import Control.Monad.Bayes.Class
import Physics.Storage (sampleBatterySpec, initBatteryState, BatteryState, BatterySpec)
import Physics.Generation (sampleGenSpec, runGen, GenSpec)
import Physics.Consumption (initConsumptionState, sampleConsumptionSpec, runConsumption, ConsumptionSpec, ConsumptionState)
import Physics.Transmission (sampleTransmissionSpec, runTransmission, TransmissionSpec, TransmissionState, initTransmissionState)
import Physics.Units (R, Sec, Meters, GeoC, EuclideanC, Watts, Amp, V, DelT, WattsPerMeterSq, MetersPerSecond, Temperature)

-- A household tracks three types of State:
-- (Storage, Consumption, Transmission)
-- This state results in a Reward at the end of an episode
type NodeId = Int

type Reward = R

type HHState = (BatteryState, TransmissionState, ConsumptionState)

data HHSpec = HHSpec
  { nId :: NodeId
  , loc :: GeoC
  , gridLoc  :: EuclideanC
  , storage  :: BatterySpec
  , generation :: GenSpec
  , consumption :: ConsumptionSpec
  } deriving (Eq, Show, Generic)

sampleHH :: (MonadSample m) => NodeId -> GeoC -> EuclideanC -> m HHSpec
sampleHH n loc grloc = do
  storage <- sampleBatterySpec
  gen <- sampleGenSpec
  consump <- sampleConsumptionSpec
  return $ HHSpec n loc grloc storage gen consump


runHH :: HHSpec -> ZonedTime -> MetersPerSecond -> Temperature -> State HHState Reward
runHH HHSpec {..} time windSpeed ambientTemp = undefined
  where
    generated = runGen loc generation time ambientTemp windSpeed
    -- (c', consumed) = runConsumption consumption
  --(t', (transmitted, lost)) <- runTransmission
  --let
  --  batteryDiff = generated + consumed + transmitted + lost
  --b' <- runBattery batteryDiff
  --put (b', t', c')

initHHState :: MonadSample m => ConsumptionSpec -> m HHState
initHHState cSpec = do
  cs <- initConsumptionState cSpec
  return (initBatteryState, initTransmissionState, cs)
  
    


{--
data NodeState = NodeState
  { time :: NodeTime
  , stored :: (Storage a => a -> WattHours)
  , chargePower :: (Storage a => a -> Watts)
  , dischargePower :: (Storage a => a -> Watts)
  , generation :: (Generator a => a -> (Watts, Sec))
  , consumption :: (Consumer a => a -> (Watts, Sec))
  } deriving (Eq, Show, Generic)
--}




{--
getGenPower :: HH -> ZonedTime -> Watts
getGenPower Node { location, generation } = sum $ map power generation
  where
    power (s, e) = radiation * s * e
    radiation :: WattsPerMeterSq
    radiation = directRadiation location (unZonedTime time)


toBatteryObs :: Amp -> V -> DelT -> BatteryObservation
toBatteryObs i v t = BatteryObservation i v t


updateSoC :: BatterySpec -> BatteryState -> Watts -> DelT -> BatteryState
updateSoC params state p t = BatteryState params state' obs' 
  where
    state' = stateNext params state obs'
    obs' = toBatteryObs v' i' t
    (v', i') = applyConstantPower p
    applyConstantPower = undefined


recieve :: Node -> Watts -> Node
recieve n p = n { storage = (updateSoC (storage n) p systemDelT) }

consume :: Node -> Demand -> Node
consume n Demand {..} = n { storage = (updateSoC (storage n) powerDraw systemDelT) }
--}
  


{---

I want a spacetime graph

Graph Construction:
Use algebraic-graphs to assemble a test graph
1. Node Features
2. Edge Features

Have functions that give you:
1) outgoing :: Node -> [Node]
2) incoming :: Node -> [Node] 

---}
