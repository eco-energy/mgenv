{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Env where


import Streamly
import Streamly.Prelude as S
import Physics.Units
import GHC.Generics (Generic)
import Control.Monad.Bayes.Class
import Control.Monad (liftM)
import Data.Functor.Rep

import GHC.Generics (Generic)

import Grid (SampledGrid, GridState, sampleGridSpec, generateGrid, initGridState, initWorldTime, gridStep)

{--
newtype Policy i p s a = Policy { policy ::(p -> State i s -> Action i a) } deriving (Generic)

instance Representable (Policy p s a) where
  type Rep (Policy p s a) = GRep
  

newtype Value i p s a = Value { value :: (p -> State i s -> Advantage) } deriving (Generic)

newtype State i s = State { state :: s } deriving (Eq, Ord, Show, Generic)

newtype Action i a = Action { action :: a } deriving (Eq, Ord, Show, Generic, Representable)

newtype Reward i s r = Reward { reward :: r } deriving (Generic, Representable)

type Advantage = R

class (Num pi, Num v, Num r) => MDP i pi v s a r where
  act :: Policy pi s a -> State s -> (Action a, Reward s r)
  advantage :: Value v s a -> Advantage
  learn ::  Advantage -> Reward s r -> Policy pi s a -> Value v s a -> (Policy pi s a, Value pi s a) 

--}
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
  let initState = initGridState grid
  --S.scanl' gridStep initState (initWorldTime startDate) 
  return ()
