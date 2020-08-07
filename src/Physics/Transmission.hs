{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Transmission
  ( TransmissionSpec(..)
  , resistance
  , TransmissionState(..)
  , initTransmissionState
  , sampleTransmissionSpec
  , runTransmission
  ) where

import Physics.Units
import GHC.Generics (Generic)
import Randomizable



data TransmissionSpec = TransmissionSpec
  { wireLength :: Meters
  , crossSection :: MetersSq
  , resistivity :: OhmMeters
  } deriving (Eq, Show, Ord, Generic)

instance Semigroup TransmissionSpec where
  (<>) ts1 ts2 = TransmissionSpec wl' cs' rvity'
    where
      wl' = l1 + l2
      cs' = weightedSum cs1 cs2
      rvity' = weightedSum rvity1 rvity2
      weightedSum a b = (scaleBy l1 a) + (scaleBy l2 b)
        where
          scaleBy l c = c * (l / l1 + l2)
      (TransmissionSpec {wireLength=l1, crossSection=cs1, resistivity=rvity1}) = ts1
      (TransmissionSpec {wireLength=l2, crossSection=cs2, resistivity=rvity2}) = ts2

instance Monoid TransmissionSpec where
  mempty = TransmissionSpec 0 0 0

instance Randomizable TransmissionSpec where
  sampleThis = sampleTransmissionSpec

resistance :: TransmissionSpec -> Ohm
resistance TransmissionSpec{..} = wireLength * resistivity / crossSection

sampleTransmissionSpec :: (MonadSample m) => m TransmissionSpec
sampleTransmissionSpec = do
  wireLength <- uniform 10 100
  diameter <- uniformD [i / 1000 | i <- [0.75..10]]
  resistivity <- normal 1.724e-8 ((1.724e-8 * 2) / 100)
  return $ TransmissionSpec wireLength (crossSection diameter) resistivity
  where
    crossSection d = pi * (d /2)**2


data Transmission = Transmission
  { v0 :: V
  , i0 :: Amp
  , v1 :: V
  } deriving (Eq, Ord, Show, Generic)

runTransmission :: TransmissionSpec -> V -> Amp -> (Watts, Watts)
runTransmission t@TransmissionSpec{} v i = (outP, loss)
  where
    loss = (i**2) * resistance t
    outP = (v*i) - loss

data TransmissionState = TransmissionState
  { power :: Watts } deriving (Eq, Show, Ord, Generic)


-- There should be a parallel Semigroup and a sequential semigroup,
-- the former should be invariant wrt the voltage and sum the current and vice versa for the latter
-- right now lets assume that the composition is parallel
instance Semigroup TransmissionState where
  (TransmissionState p1) <> (TransmissionState p2) = TransmissionState $ p1 + p2
  

instance Monoid TransmissionState where
  mempty = initTransmissionState
  

initTransmissionState :: TransmissionState
initTransmissionState = TransmissionState 0
