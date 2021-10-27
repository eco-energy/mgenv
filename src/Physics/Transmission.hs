{-# LANGUAGE NamedFieldPuns, TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, TypeOperators #-}
module Physics.Transmission
  ( TransmissionSpec(..)
  , resistance
  , TransmissionState(..)
  , Transmission(..)
  , sampleTransmissionSpec
  , sendTx
  , recieveTx
  ) where

import Control.Applicative
import Physics.Units
import GHC.Generics (Generic)
import Prob.Randomizable
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Pipe (Pipe(..))
import Streamly.Internal.Data.Pipe as P
import ConCat.Isomorphism

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


type VI v i = (v, i)
type VIDel v i = VI v i <-> (v, v, i)


viDel :: (Num v, Num i) => VIDel v i
viDel =  fwd :<-> rev 
  where
    fwd = \(v, i) -> (v - 0, 0, i)
    rev = \(v, v', i) -> (v - v', i)

optimiseVI :: p -> TransmissionSpec -> VI v i
optimiseVI w tx = undefined
  where
    r = resistance tx 

type PowerIso p v i = p <-> (VI v i)

powerIso :: forall p v i. (Num p, Fractional p, RealFrac p, RealFrac v, RealFrac i)
  => TransmissionSpec -> PowerIso p v i
powerIso ts = (fwd :<-> rev)
  where
    fwd w = optimiseVI w ts
    rev :: (v, i) -> p
    rev (v, i) = (realToFrac v) * (realToFrac i)



newtype Transmission = Transmission
  { unTransmission :: PowerIso Watts V Amp
  } deriving (Generic)

sendTx :: (Monad m) => Transmission  -> Pipe m Watts (V, Amp)
sendTx = P.map . isoFwd . unTransmission

recieveTx :: (Monad m) => Transmission  -> Pipe m (V, Amp) Watts
recieveTx = P.map . isoRev . unTransmission

-- runTransmission :: forall t m. (S.IsStream t, S.MonadAsync m)
--   => TransmissionSpec -> Transmission m a -> t m a
-- runTransmission t Transmission{v0, i0, v1} = S.zipWith (,) outP loss
--   where
--     v :: t m V
--     v = liftA2 (-) v0 v1
--     loss :: t m Watts
--     loss = fmap (\i -> i**2 * (resistance t)) i0
--     outP :: t m Watts
--     outP = (S.zipWith (-) (S.zipWith (*) v i0) loss)

newtype TransmissionState = TransmissionState (TransmissionSpec, Transmission)
  deriving (Generic)

