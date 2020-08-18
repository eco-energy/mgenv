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
module Grid.MDP where

import GHC.Generics (Generic)

import ConCat.Misc

import Data.Monoid
import qualified Algebra.Graph as G
import Algebra.Graph.Class ()

import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S

import Env.MonadEnv
import RL.MDP hiding (Policy)

import Physics

newtype Generation = Generation R
  deriving (Eq, Ord, Show, Generic, Num)

newtype Storage v i = Storage (i -> v)
  deriving (Generic)

newtype Demand = Demand R
  deriving (Eq, Ord, Show, Generic, Num)

data Node' = Node'
  { vB :: R
  , iB :: R
  , iL :: R
  , vG :: R
  , iG :: R
  } deriving (Eq, Ord, Show, Generic)

data NodeHistory = NodeH
  { stored :: R
  , consumed :: R
  , generated :: R
  , transmitted :: R
  } deriving (Eq, Ord, Show, Generic)

nextState :: Node' -> Gen -> Demand -> Tx -> Node'
nextState = undefined

nextHist :: NodeHistory -> Node' -> NodeHistory
nextHist = undefined

gridReward :: HistoryG -> Double
gridReward ks = let (Sum r) = G.foldg (0) rk (<>) (<>) ks
                    in r
  where
    rk :: NodeHistory -> Reward
    rk x = Sum $ (stored x) + (consumed x + generated x) + transmitted x

--k = G.foldg (Sum 0) rk (<>)

newtype Tx = Tx R
  deriving (Eq, Ord, Show, Generic, Num)

newtype Gen = Gen R
  deriving (Eq, Ord, Show, Generic, Num)

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

type GridMDP = MarkovDecisionProcess MonadEnv (StateG :* HistoryG) ActionG


microgridMDP :: MonadEnv DemandG -> MonadEnv GenG -> GridMDP
microgridMDP demand gen = MDP
  { act = \(st, hist) a -> do
      d <- lift demand
      g <- lift gen
      reward (gridReward hist)
      let st' = nextState <$> st <*> g <*> d <*> a
          hist' = nextHist <$> hist <*> st'
      return $ (st', hist')
  }


envpolicy :: ((StateG :* HistoryG) -> ActionG) -> MarkovRewardProcess MonadEnv (StateG :* HistoryG)
envpolicy p = apply p (microgridMDP demandG genG)

demandG :: (MonadSample m) => m DemandG
demandG = undefined -- (Demand =<< normal 200 100)

genG :: (MonadSample m) => m GenG
genG = undefined

p :: GraphPolicy p m
p = undefined

p' :: (StateG :* HistoryG) -> ActionG
p' = undefined

run :: SerialT MonadEnv (Reward :* (StateG :* HistoryG))
run = S.runStateT r0 $ simulate (envpolicy p') (G.empty, G.empty)
  where
    r0 :: Reward
    r0 = Sum 0
