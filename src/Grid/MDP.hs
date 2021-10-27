{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingVia, DerivingStrategies, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Grid.MDP where

import GHC.Generics (Generic)

import ConCat.Misc

import Data.Monoid
import qualified Algebra.Graph as G
import Algebra.Graph.Class ()

import Control.Applicative
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Env.MonadEnv
import RL.MDP hiding (Policy)

import Physics

newtype Generation = Generation R
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, Floating, RealFloat)

newtype Storage v i = Storage (i -> v)
  deriving (Generic)

newtype Demand = Demand R
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, Floating, RealFloat)

data Node' = Node'
  { vB :: R
  , iB :: R
  , vT :: R
  , iT :: R
  , iL :: R
  , vL :: R
  , vG :: R
  , iG :: R
  } deriving (Eq, Ord, Show, Generic)

data NodeHistory = NodeH
  { stored :: R
  , consumed :: R
  , generated :: R
  , transmitted :: R
  } deriving (Eq, Ord, Show, Generic)


diffHist :: NodeHistory -> NodeHistory -> Node'
diffHist n n' = Node'{..}
  where
    (vB, iB) = batteryMod n n'
    (vT, iT) = p2vi (transmitted n) (transmitted n')
    (vG, iG) = p2vi (generated n) (generated n')
    (iL, vL) = p2vi (consumed n) (consumed n')
    p2vi !p !p' = let
      delP = p - p'
      v = delP / i
      i = delP / v
      in (v, i)
    batteryMod n n' = (12, (stored n - stored n') / 12)
      
    
gridReward :: HistoryG -> Double
gridReward ks = let (Sum r) = G.foldg (0) rk (<>) (<>) ks
                    in r
  where
    rk :: NodeHistory -> Reward
    rk x = Sum $ (stored x) + (consumed x + generated x) + transmitted x

--k = G.foldg (Sum 0) rk (<>)

newtype Tx = Tx R
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, Floating, RealFloat)

newtype Gen = Gen R
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, Floating, RealFloat)

type PolicyG = G.Graph (Node' :* NodeHistory -> Tx)

type HistoryG = G.Graph NodeHistory

type StateG = G.Graph Node'

type ActionG = G.Graph Tx

type RewardG = G.Graph Reward

type GenG = G.Graph Gen

type DemandG = G.Graph Demand

type Skeleton = G.Graph Int

newtype Policy p m s a = Policy { runPolicy :: p -> s -> m (p, a) }

type GraphPolicy p m = Policy p m (StateG :* HistoryG) ActionG

-- linearPolicy :: (MonadEnv m) => Policy p m (Z s a b) (Z s a b)
-- linearPolicy = Policy p
--   where
--     p par state = return

type GridMDP = MarkovDecisionProcess MonadEnv (StateG :* HistoryG) ActionG



nextHist :: Gen -> Demand -> Tx -> Unop NodeHistory
nextHist (Gen g) (Demand d) (Tx tx) n1 = n1
  { stored = (stored n1 + g)
  , consumed = d
  , generated = g
  , transmitted = tx
  }

onG :: GenG -> DemandG -> ActionG -> HistoryG -> HistoryG
onG g d a h = nextHist <$> g <*> d <*> a <*> h

microgridMDP :: MonadEnv DemandG -> MonadEnv GenG -> GridMDP
microgridMDP demand gen = MDP
  { act = \(st, hist) tx' -> do
      d <- lift demand
      g <- lift gen
      let tx = tx' (st, hist) 
      let st' = liftA2 diffHist hist (hist') 
          hist' :: HistoryG
          hist' = onG g d tx hist
      reward (gridReward hist')
      return $ (st', hist')
  }


envReward :: Skeleton -> ((StateG :* HistoryG) -> ActionG) -> MarkovRewardProcess MonadEnv (StateG :* HistoryG)
envReward skeleton p = apply p $ microgridMDP (demandG skeleton) (genG skeleton)

demandG :: forall m. (MonadSample m) => Skeleton -> m DemandG
demandG s = sequence g
  where
    g :: G.Graph (m Demand)
    g = fmap (\_ -> Demand <$> (normal 200 100)) s

deriving instance Foldable G.Graph
deriving instance Traversable G.Graph

genG :: forall m. (MonadSample m) => Skeleton -> m GenG
genG s = sequence g
  where
    g :: G.Graph (m Gen)
    g = fmap (\_ -> Gen <$> normal 200 100) s

p :: Policy p m (StateG :* HistoryG) ActionG
p = undefined

p' :: (StateG :* HistoryG) -> ActionG
p' = undefined

run :: Skeleton -> SerialT MonadEnv (Reward :* (StateG :* HistoryG))
run s = S.runStateT (pure r0) $ simulate (envReward s p') (G.empty, G.empty)
  where
    r0 :: Reward
    r0 = Sum 0
