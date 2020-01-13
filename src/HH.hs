{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module HH
  ( sampleHH
  , HHSpec
  , gridLoc
  , NodeId
  , HHState
  , hhStep
  ) where

import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time

-- work with the functions available through C as if you have Category Instances.
import qualified ConCat.CircAff as C

import GHC.Generics (Generic)

import Control.Monad.State

import Control.Monad.Bayes.Class


-- In this universe, there is no State.
-- There is a category of l-graph cospans that compose if the start or end of a circuit meets another.
-- we have id and (.).
-- We also have swap from braided, a monoidal sum and product.
-- But when do the cospans compose?
-- We have to express a high-level circuit in a formal semantic that is concise and as flexible as drawing squiggly lines on paper.
-- Obviously it has to be completely declarative and tersely functional at the same time.
--
-- The battery has a hysterises voltage.
-- 
import Physics.Storage
  ( sampleBatterySpec
  , BatteryState
  , BatterySpec
  , batteryVoltage
  , stateNext
  , energyStored
  )

import Physics.Generation
  ( sampleGenSpec
  , runGen
  , GenSpec
  )
  
import Physics.Transmission
  ( TransmissionState
  , initTransmissionState
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

type HHState = (BatteryState, ConsumptionState)

data HHSpec = HHSpec
  { nId :: NodeId
  , loc :: GeoC
  , gridLoc  :: EuclideanC
  , storage  :: BatterySpec
  , generation :: GenSpec
  , consumption :: ConsumptionSpec
  } deriving (Eq, Show, Generic)

instance Ord HHSpec where
  a `compare` b = (nId a) `compare` (nId b)


sampleHH :: (MonadSample m) => NodeId -> GeoC -> EuclideanC -> m HHSpec
sampleHH n loc grloc = do
  storage <- sampleBatterySpec
  gen <- sampleGenSpec
  consump <- sampleConsumptionSpec
  return $ HHSpec n loc grloc storage gen consump


data Audit = Audit
  { transmittedIn :: Watts
  , transmittedOut :: Watts
  , consumed :: Watts
  , generated :: Watts
  , tDiff :: Time.DiffTime
  } deriving (Eq, Show, Ord, Generic)


data Transaction = Transaction
  { start :: Time.UTCTime,
    duration   :: Time.DiffTime,
    edges :: [C.Edge Double (NodeId, C.VI Double)]
  } deriving (Eq, Ord, Show)


data Transmission = Transmission Watts Amp


hhStep' :: (MonadSample m) => HHSpec -> DelT -> ZonedTime -> MetersPerSecond -> Temperature -> Transmission -> StateT HHState m Reward
hhStep' HHSpec {..} delT time windSpeed ambientTemp transmission = do
  (bs, cs) <- get
  (cs', consumed) <- runConsumption cs
  let
    (Transmission _ tc) = transmission
    generated = runGen loc generation (unZonedTime time) ambientTemp windSpeed
    bV = batteryVoltage bs
    generationCurrent = generated / bV
    consumptionCurrent = consumed / bV
    batteryCurrent = tc + generationCurrent + consumptionCurrent
    bs' = stateNext storage bs batteryCurrent delT 
    reward = (energyStored bs') + consumed
  put (bs', cs')
  return reward

hhStep :: (MonadSample m) => HHSpec -> (DelT -> ZonedTime -> MetersPerSecond -> Temperature -> Transmission -> StateT HHState m Reward)
hhStep hspec = hhStep' hspec



{--
recieve :: Node -> Watts -> Node
recieve n p = n { storage = (updateSoC (storage n) p systemDelT) }

consume :: Node -> Demand -> Node
consume n Demand {..} = n { storage = (updateSoC (storage n) powerDraw systemDelT) }


delT = timeDiffInSeconds time tp
timeDiffInSeconds :: ZonedTime -> ZonedTime -> DelT
timeDiffInSeconds = undefined

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
