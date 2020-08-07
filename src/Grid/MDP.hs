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

module Grid.MDP where

import GHC.Generics (Generic)

import ConCat.Misc

import Data.Monoid
import qualified Algebra.Graph as G
--import qualified Algebra.Graph.Labelled as LG
import Algebra.Graph.Class ()

import Control.Monad.Trans.State.Strict ()
import Control.Monad.Trans.Class

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S

import RL.MDP


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

gridReward :: GridHistory -> Double
gridReward ks = let (Sum r) = G.foldg (0) rk (<>) (<>) ks
                    in r
  where
    rk :: NodeHistory -> Reward
    rk x = Sum $ (stored x) +  (consumed x + generated x) + transmitted x

--k = G.foldg (Sum 0) rk (<>)

newtype Tx = Tx R
  deriving (Eq, Ord, Show, Generic, Num)

newtype Gen = Gen R
  deriving (Eq, Ord, Show, Generic, Num)

type GridHistory = G.Graph NodeHistory

type StateGraph = G.Graph Node'

type DemandGraph = G.Graph Demand

type ActionGraph = G.Graph Tx

type RewardGraph = G.Graph Reward

type GenGraph = G.Graph Gen

type GraphPolicy = Policy (StateGraph :* GridHistory) ActionGraph


microgridMDP :: (Monad m) => m DemandGraph -> m GenGraph -> MarkovDecisionProcess m (StateGraph :* GridHistory) ActionGraph
microgridMDP demand gen = MDP
  { act = \(st, hist) a -> do
      d <- lift demand
      g <- lift gen
      reward (gridReward hist)
      let st' = nextState <$> st <*> g <*> d <*> a
          hist' = nextHist <$> hist <*> st'
      return $ (st', hist')
  }


dg :: m DemandGraph
dg = undefined

gg :: m GenGraph
gg = undefined

mgR :: (Monad m) => GraphPolicy -> MarkovRewardProcess m (StateGraph :* GridHistory)
mgR = (flip apply) (microgridMDP dg gg) 

p :: GraphPolicy
p = undefined

run :: (MonadAsync m) => SerialT m (Reward :* (StateGraph :* GridHistory))
run = S.runStateT r0 $ simulate (mgR p) (G.empty, G.empty)
  where
    r0 :: Reward
    r0 = Sum 0
