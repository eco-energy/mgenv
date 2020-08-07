{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Env where


import Streamly
import Streamly.Prelude as S
import Physics.Units (GeoC, ZonedTime, MetersPerSecond, Temperature)
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class
import Control.Monad (liftM)
import Data.Functor.Rep

import GHC.Generics (Generic)

import Grid (SampledGrid, GridState, sampleGridSpec, generateGrid, initGridState, initWorldTime, gridStep)


-- This should be at grid level
data EnvCond = EnvCond
  { windSpeed :: MetersPerSecond
  , ambientTemp :: Temperature
  } deriving (Eq, Ord, Show, Generic)


sampleEnvCond :: (MonadSample m) => m EnvCond
sampleEnvCond = do
  ws <- liftM abs $ normal 1 5
  aT <- normal 20 10
  return $ EnvCond ws aT



runEnv :: (MonadSample m) => ZonedTime -> GeoC -> m ()
runEnv startDate centerPoint = do
  gridSpec <- sampleGridSpec
  grid <- generateGrid gridSpec
  let initState = initGridState grid
  --S.scanl' gridStep initState (initWorldTime startDate) 
  return ()
