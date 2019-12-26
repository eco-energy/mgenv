{-# LANGUAGE DeriveGeneric #-}

module Env where


import Streamly
import Streamly.Prelude as S
import Physics.Units
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class
import Control.Monad (liftM)

import Grid (SampledGrid, GridState, sampleGridSpec, generateGrid, initGridState, initWorldTime, gridStep)

--import RL.PPO (Agent (..))


{--
setupDay :: GeoC -> ZonedTime -> (ZonedTime, ZonedTime)
setupDay loc day = (start, end)
  where
    (start, end) = sunRiseAndSet loc verticalShift lcd
    lcd = zonedTimeToLCD day
    verticalShift = 0.833333
--}

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


getGridState = undefined
getGridSpec = undefined

runEnv startDate centerPoint = do
  gridSpec <- sampleGridSpec
  grid <- generateGrid gridSpec
  let
    initState = initGridState gridSpec
  --S.scanl' gridStep initState (initWorldTime startDate) 
  return ()
