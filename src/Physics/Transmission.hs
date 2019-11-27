{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Transmission where

import Physics.Units
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class

class Transmission a where
  thing :: a

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

sampleTransmissionSpec :: (MonadSample m) => m TransmissionSpec
sampleTransmissionSpec = do
  wireLength <- uniform 10 100
  diameter <- uniformD [i / 1000 | i <- [0.75..10]]
  resistivity <- normal 1.724e-8 ((1.724e-8 * 2) / 100)
  let
    crossSection = pi * (diameter / 2)**2
    resistance = (wireLength * resistivity) / crossSection
  return $ TransmissionSpec wireLength crossSection resistivity resistance


runTransmission :: TransmissionSpec -> V -> Amp -> (Watts, Watts)
runTransmission TransmissionSpec {resistance} v i = (outP, loss)
  where
    loss = (i**2) * resistance
    outP = (v*i) - loss

data TransmissionState = TransmissionState
  { current :: Amp, voltage :: V } deriving (Eq, Show, Ord, Generic)
