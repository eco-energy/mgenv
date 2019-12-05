{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Transmission (TransmissionSpec, TransmissionState, initTransmissionState, sampleTransmissionSpec, runTransmission) where

import Physics.Units
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class


{--
data EdgeState = EdgeState
  { resistance :: Ohm
  , vN1 :: V
  , vN2 :: V
  } deriving (Generic)

edgePower :: EdgeState -> Watts
edgePower EdgeState {..} = (vN1 * (vN1 - vN2)) / resistance

run = undefined
--}

p2pcurrent :: V -> V -> Ohm -> Amp
p2pcurrent v1 v2 r = (v1 - v2) / r

data TransmissionSpec = TransmissionSpec
  { wireLength :: Meters
  , crossSection :: MetersSq
  , resistivity :: OhmMeters
  , resistance :: Ohm
  } deriving (Eq, Show, Ord, Generic)

instance Semigroup TransmissionSpec where
  (<>) ts1 ts2 = TransmissionSpec wl' cs' rvity' r'
    where
      wl' = l1 + l2
      cs' = avg cs1 cs2
      rvity' = avg rvity1 rvity2
      r' = wResistance wl' cs' rvity'
      avg a b = (a + b / 2)
      (TransmissionSpec {wireLength=l1, crossSection=cs1, resistivity=rvity1}) = ts1
      (TransmissionSpec {wireLength=l2, crossSection=cs2, resistivity=rvity2}) = ts2

instance Monoid TransmissionSpec where
  mempty = TransmissionSpec 0 0 0 0
  

sampleTransmissionSpec :: (MonadSample m) => Meters -> m TransmissionSpec
sampleTransmissionSpec wireLength = do
  diameter <- uniformD [i / 1000 | i <- [0.75..10]]
  resistivity <- normal 1.724e-8 ((1.724e-8 * 2) / 100)
  let
    crossSection = pi * (diameter / 2)**2
    r = wResistance wireLength crossSection resistivity
  return $ TransmissionSpec wireLength crossSection resistivity r


wResistance :: Meters -> OhmMeters -> MetersSq -> Ohm
wResistance wireLength resistivity crossSection = (wireLength * resistivity) / crossSection

data Transmission = Transmission
  { v0 :: V
  , i0 :: Amp
  , v1 :: V
  } deriving (Eq, Ord, Show, Generic)

runTransmission :: TransmissionSpec -> V -> Amp -> (Watts, Watts)
runTransmission TransmissionSpec {resistance} v i = (outP, loss)
  where
    loss = (i**2) * resistance
    outP = (v*i) - loss

data TransmissionState = TransmissionState
  { current :: Amp, voltage :: V } deriving (Eq, Show, Ord, Generic)

initTransmissionState :: TransmissionState
initTransmissionState = TransmissionState 0 0
