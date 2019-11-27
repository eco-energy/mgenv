{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Consumption where

import Physics.Units
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class
import Control.Monad (replicateM, liftM, replicateM)

data Load = Load
  { power :: Watts
  , utility :: R
  , isRunning :: Bool
  } deriving (Eq, Ord, Show, Generic)


newtype ConsumptionSpec = ConsumptionSpec [Load] deriving (Eq, Ord, Show, Generic)

sampleLoadSpec :: (MonadSample m) => m Load
sampleLoadSpec = do
  p <- uniformD [1..100]
  u <- liftM abs $ normal 0.5 0.2
  hot <- bernoulli u
  return $ Load p u hot

sampleConsumptionSpec :: MonadSample m => m ConsumptionSpec
sampleConsumptionSpec = do
  n <- uniformD [1..10]
  loads <-  (replicateM n sampleLoadSpec)
  return $ ConsumptionSpec $ loads

runLoad :: Load -> V -> Amp
runLoad Load {..} batteryV = power / batteryV

runConsumption :: (MonadSample m) => ConsumptionSpec -> m (ConsumptionSpec, Watts)
runConsumption (ConsumptionSpec loads) = do
  let
    updateState :: (MonadSample m) => Load -> m Load
    updateState l = do
      newState <- bernoulli (utility l)
      return l {isRunning=newState}
  cs' <-  mapM updateState loads
  let
    consumed = sum $ map power $ filter isRunning loads
  return (ConsumptionSpec $ cs', consumed)
