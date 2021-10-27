{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, TupleSections, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module Physics.Consumption
  ( ConsumptionSpec(..)
  , sampleConsumptionSpec
  , ConsumptionState(..)
  , initConsumptionState
  , initConsumptionStateM
  , consumptionS
  , Load(..)
  , LoadState(..)
  ) where

import qualified Streamly.Prelude as S
import Physics.Units
import GHC.Generics (Generic)
import Control.Monad (replicateM, liftM, replicateM, mapM)

import Prob.Randomizable

data Load = Load
  { power :: Watts
  , utility :: R
  } deriving (Eq, Ord, Show, Generic)

instance Randomizable Load where
  sampleThis = sampleLoadSpec


data LoadState = LoadState
  { load :: Load
  , isRunning :: Bool
  } deriving (Eq, Ord, Show, Generic) 

newtype ConsumptionSpec = ConsumptionSpec [Load] deriving (Eq, Ord, Show, Generic)

newtype ConsumptionState = ConsumptionState [LoadState] deriving (Eq, Ord, Show, Generic)


instance Randomizable ConsumptionSpec where
  sampleThis = sampleConsumptionSpec

sampleLoadSpec :: (MonadSample m) => m Load
sampleLoadSpec = do
  p <- uniformD [5, 10..200]
  u <- liftM abs $ normal 0.5 0.2
  return $ Load p u

sampleConsumptionSpec :: MonadSample m => m ConsumptionSpec
sampleConsumptionSpec = do
  n <- uniformD [1..10]
  loads <-  (replicateM n sampleLoadSpec)
  return $ ConsumptionSpec $ loads

runLoad :: Load -> V -> Amp
runLoad Load {..} batteryV = power / batteryV

initConsumptionStateM :: (MonadSample m) => ConsumptionSpec -> m ConsumptionState
initConsumptionStateM (ConsumptionSpec loads) = do
  let
    hot Load {utility} = bernoulli utility
  states <- mapM hot loads
  return $ ConsumptionState (map (\(l, s)-> LoadState l s) $ zip loads states) 

initConsumptionState :: ConsumptionSpec -> ConsumptionState
initConsumptionState (ConsumptionSpec loads) = ConsumptionState ((\l -> LoadState l False) <$> loads)

consumed :: ConsumptionState -> Watts
consumed (ConsumptionState loads) = sum $ map (power . load) $ filter isRunning loads

consumptionS :: forall t m. (S.IsStream t, S.MonadAsync m, MonadSample m)
  => ConsumptionSpec
  -> t m (ConsumptionState, Watts)
consumptionS spec = fmap (\a -> (a, consumed a)) $ S.iterateM (updateC) initS
  where
    initS :: m (ConsumptionState)
    initS = initConsumptionStateM spec
    updateState :: LoadState -> m LoadState
    updateState l = do
      newState <- bernoulli ((utility . load) l)
      return l {isRunning=newState}
    updateC :: (ConsumptionState) -> m (ConsumptionState)
    updateC (ConsumptionState cs) = do
      cs' <- mapM updateState cs
      return (ConsumptionState cs')
