{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module HH where

import Physics.Storage (sampleBatterySpec, BatteryState)
import Physics.Generation (sampleGenSpec, runGen)
import Physics.Consumption (sampleLoadSpec)
import Physics.Transmission (sampleTransmissionSpec, TransmissionState)
import Physics.Units (R, Sec, Meters, GeoC, EuclideanC, ZonedTime, unZonedTime, Watts, Amp, V, DelT, WattsPerMeterSq)

import Algebra.Graph.AdjacencyIntMap.Algorithm
import Algebra.Graph.AdjacencyIntMap
import GHC.Generics (Generic)


-- A household tracks three types of State:
-- (Storage, Consumption, Transmission)
-- This state results in a Reward at the end of an episode


type Reward = R

type HHState = State (Battery, TransmissionState, ConsumptionState)

data HH = HH
  { location :: GeoC
  , gridLoc  :: EuclideanC
  , storage  :: BatterySpec
  , generation :: GeneratorSpec
  , consumption :: LoadSpec
  } deriving (Eq, Show, Generic)


-- This should be at grid level
data EnvCond = EnvCond
  { windSpeed :: MetersPerSecond
  , ambientTemp :: R
  } deriving (Eq, Ord, Show, Generic)


setupDay :: GeographicCoordinates -> ZonedTime -> RiseSetMB
setupDay loc day = sunRiseAndSet loc verticalShift lcd
  where
    lcd = zonedTimeToLCD day
    verticalShift = 0.833333


sampleEnvCond :: (MonadSample m) => m EnvCond
sampleEnvCond = do
  windSpeed <- liftM abs $ normal 1 5
  ambientTemp <- normal 20 10
  return $ EnvCond windSpeed ambientTemp


data Demand = Demand { powerDraw :: Watts}

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

type ConvEff = R
type MetersSq = R

getGenPower :: HH -> ZonedTime -> Watts
getGenPower Node { location, generation } = sum $ map power generation
  where
    power (s, e) = radiation * s * e
    radiation :: WattsPerMeterSq
    radiation = directRadiation location (unZonedTime time)


toBatteryObs :: Amp -> V -> DelT -> BatteryObservation
toBatteryObs i v t = BatteryObservation i v t


updateSoC :: BatterySpec -> BatteryState -> Watts -> DelT -> Battery
updateSoC params state p t = Battery params state' obs' 
  where
    state' = stateNext params state obs'
    obs' = toBatteryObs v' i' t
    (v', i') = applyConstantPower p
    applyConstantPower = undefined


recieve :: Node -> Watts -> Node
recieve n p = n { storage = (updateSoC (storage n) p systemDelT) }

consume :: Node -> Demand -> Node
consume n Demand {..} = n { storage = (updateSoC (storage n) powerDraw systemDelT) }

  


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
