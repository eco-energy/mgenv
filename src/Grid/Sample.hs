{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies, DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}


module Grid.Sample
  ( GridState(..)
  , SampledGrid(..)
  , GridSpec' (..)
  , GridSpec
  , sampleGridSpec
  , generateGrid
  , initGridState
  ) where

import Control.Monad.Bayes.Class
import Control.Monad (replicateM, liftM)

import GHC.Generics (Generic)
import Data.Generics.Product
import Data.Generics.Sum

import Control.Lens
import Physics.Units
  ( GeoC
  , R
  , Meters
  , MetersPerSecond
  , Temperature
  , EuclideanC
  , BearingDeg
  , Watts
  , ZonedTime
  , location
  , reverseHaversine
  , incrementTime
  , dateStartToUTC
  , fromUTC
  , absToUTC
  )


import RL.MDP
import Env.MonadEnv
import Prob.Randomizable
import ConCat.Misc hiding (R)
import Control.Monad.Identity
import Data.Time (Day, UTCTime(..), NominalDiffTime, fromGregorian)
import Grid.HH (initHHState, HHSpec(..), sampleHH, NodeId, HHState(..), hhS, runHH)
import Physics.Transmission
import qualified Data.List.NonEmpty as NE
import Geometry.EMST (minSpanTreeEdges, positiveGridPoints)
import qualified Data.Map as Map

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as P

import Data.Bifunctor
import qualified Algebra.Graph.Labelled as LG

import Streamly.Internal.Data.Time.Units

{-
Semantically, what is the purpose of the graph here?
--}


newtype Grid e n = Grid
  { unGrid :: LG.Graph e n
  } deriving (Eq, Show, Generic, Functor, Bifunctor)


initWorldTime :: (MonadAsync m) => ZonedTime -> SerialT m ZonedTime 
initWorldTime initTime = S.iterate tn initTime
  where
  tn = incrementTime (1 :: NominalDiffTime)


data GridSpec' m = GridSpec
  { startDate :: m UTCTime
  , rate :: m RelTime
  , geometricOrigin :: m GeoC
  , nNodes :: m Int
  , nodeDist :: m Meters
  } deriving (Generic)


type GridSpecE = GridSpec' MonadEnv
type GridSpec = GridSpec' Identity

deriving instance Eq (GridSpec' Identity)
-- deriving instance Ord (GridSpec' Identity)
deriving instance Show (GridSpec' Identity)

instance Randomizable (GeoC)

instance Randomizable Coord

instance Randomizable LifeTime



instance (MonadSample m) => Randomizable (GridSpec' m) where
  sampleThis = do
    coords <- sampleThis
    lifeTime <- sampleThis
    return $ sampleGridSpec (pure coords) (pure lifeTime)


newtype SampledGrid = SampledGrid (LG.Graph TransmissionSpec HHSpec) deriving (Eq, Show, Generic)

newtype GridState m = GridState (LG.Graph (UF.Unfold TransmissionState  ) (NodeId, HHState t m))
  deriving (Generic)
  deriving newtype (Functor, Monad, Applicative)

-- instance Functor GridState

evolveGridState :: (((x -> s) -> x) -> s)
evolveGridState = unGridState

--runG :: LG.Graph (TransmissionState t m) (HHStat
--runG g = LG.foldg

newtype GridAction = GridAction (LG.Graph () Double)

--runGrid :: (IsStream t, MonadAsync m) => GridState t m -> t m (GridState t m, GridAction)
--runGrid initState = S.iterateM stepGridState (pure $ (initState, actor f initState))

--stepGridState :: (GridState t m, GridAction) -> m (GridState t m, GridAction)
--stepGridState ((GridState s), (GridAction a)) = liftA2 (TransmissionState, HHState, HHState)

f :: ([HHState t m], HHState t m) -> TransmissionState t m
f = undefined

actor :: (([HHState t m], HHState t m) -> TransmissionState t m) -> GridState t m -> GridAction
actor = undefined


data Node = Node
  { node :: NodeId
  , coords :: EuclideanC
  , geoCoords :: GeoC
  } deriving (Eq, Show, Generic)

instance Ord Node where
  (Node n1 _ _) `compare` (Node n2 _ _) = n1 `compare` n2


mkSampledGrid :: LG.Graph TransmissionSpec HHSpec -> SampledGrid
mkSampledGrid = SampledGrid

type LifeTime = Finite :+ Infinite


newtype Finite = Finite (AbsTime :* AbsTime :* RelTime)
  deriving (Eq, Ord, Show, Generic) 

newtype Infinite = Infinite (AbsTime :* RelTime)
  deriving (Eq, Ord, Show, Generic)


data Coord = GPS !GeoC
           | R2 !Meters !Meters
           | R3 !Meters !Meters !Meters
           | RRTheta !Meters !Meters !BearingDeg
           | RThetaTheta !Meters !BearingDeg !BearingDeg


type GridSeed = (Coord, LifeTime)

infiniteStartDate :: (MonadSample m) => Infinite -> m UTCTime
infiniteStartDate (Infinite (s, dt)) = pure (absToUTC s)

finiteStartDate :: (MonadSample m) => Finite -> m UTCTime
finiteStartDate (Finite ((s, e), dt)) = dateStartToUTC
  <$> (uniformD $ enumFromTo (utctDay . absToUTC $ s) (utctDay . absToUTC $ e))

delta :: LifeTime -> RelTime
delta (Left (Finite (_, dt))) = dt
delta (Right (Infinite (_, dt))) = dt

sampleGridSpec :: MonadSample m => m LifeTime -> m GeoC -> GridSpec' m
sampleGridSpec lifeTime cp = GridSpec
  { startDate = either finiteStartDate infiniteStartDate =<< lifeTime 
  , rate = delta <$> lifeTime
  , geometricOrigin = cp
  , nNodes = uniformD [10..10000]
  , nodeDist = do
      mean <- (normal 20 60)
      std <- (normal 10 20)
      normal mean std
  }

getSpec :: (MonadSample m) => GridSpec' m -> m (GridSpec)
getSpec GridSpec{..} = GridSpec
    <$> (pure <$> startDate)
    <*> (pure <$> rate)
    <*> (pure <$> geometricOrigin)
    <*> (pure <$> nNodes)
    <*> (pure <$> nodeDist) 
  

-- defGridSpec :: GridSpec
-- defGridSpec = GridSpec
--   (pure . fromUTC $ dateStartToUTC $ fromGregorian 2020 05 01)
--   (pure . location 24.54743000 67.62771000)
--   (pure 3)
--   (normal 10 5)
--   5

-- initGridState :: (IsStream t, MonadAsync m, MonadSample m) => SampledGrid -> GridState t m
-- initGridState (SampledGrid gs) = GridState $ bimap txInit hhInit gs
--   where
--     txInit tx
--       | tx == mempty = mempty
--       | otherwise = TransmissionState 0.001
--     hhInit HHSpec{consumption} = HHState S.nil S.nil S.nil S.nil

toHH :: (MonadSample m) => Node -> m HHSpec
toHH (Node{node, geoCoords, coords}) = sampleHH node geoCoords coords


type NodeDict = Map.Map NodeId Node

type Edge = (NodeId, NodeId)

generateGrid :: (MonadSample m) => GridSpec' m -> m (SampledGrid)
generateGrid GridSpec {..} = do
  ns <- nNodes
  go <- geometricOrigin
  angularCoords <- (uncurry zip) <$> ((,)
    <$> (replicateM ns $ nodeDist)
    <*> (replicateM ns $ uniform 0 360))
  let
    (nodeDict, tedges) = localAndGlobalLoc go angularCoords
    edgeLoc :: Edge -> (Node, Node)
    edgeLoc (x, y) = ((nodeDict Map.! x), (nodeDict Map.! y))
  gEdges <- mapM (uncurry sampleEdge) $ map edgeLoc tedges
  return $ mkSampledGrid $ LG.edges gEdges

dup :: a -> (a, a)
dup a = (a, a)

uc2 :: forall a b c f. (a -> b -> c -> f) -> ((a, b), c) -> f
uc2 = uncurry . uncurry

type AngularCoords = (Meters, BearingDeg)
type Graph' = LG.Graph Edge Node
type Graph = (NodeDict, [Edge]) 

localAndGlobalLoc :: GeoC -> [AngularCoords] -> Graph
localAndGlobalLoc center angularCoords = (nodeDict, tedges)
  where
    radials = map (\a -> (radial a center)) angularCoords
    ix = zip [(0::NodeId)..] $ positiveGridPoints angularCoords
    xs = zipWith (\geoC (i, p) -> ((i, geoC), p)) radials ix
    edges :: [(NodeId, NodeId)]
    edges = minSpanTreeEdges $ NE.fromList ix
    nodeDict :: Map.Map NodeId Node
    nodeDict = Map.fromList $ (bimap node id . dup . (uncurry . uncurry $ mkNode)) <$> xs

mkNode :: Int -> GeoC -> EuclideanC -> Node
mkNode !i !g !c = Node i c g

sampleEdge :: (MonadSample m) => Node -> Node -> m (TransmissionSpec, HHSpec, HHSpec)
sampleEdge loc1 loc2 = (,,) <$> sampleTransmissionSpec <*> (toHH loc1) <*> (toHH loc2) 

distance :: Node -> Node -> R
distance loc1 loc2 = dist (coords loc1) (coords loc2)

dist :: (R, R) -> (R, R) -> R 
dist (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2

norm = dist

type Radial = Meters :* BearingDeg

-- $ get the gps coordinate of a point a distance and at an angle away from another
radial ::  Radial -> GeoC -> GeoC
radial (m, b) g = reverseHaversine g m b


-- runGrid :: (MonadSample m) => m LifeTime -> m GeoC -> GridState t m
-- runGrid lifeTime origin = do
--   envCond <- sampleEnvCond
--   S.iterate runHH 
--   where
--     gridSpec = (generateGrid (sampleGridSpec lifeTime origin))


-- iterateState :: GridState t m -> GridState t m
-- iterateState () = GridState

-- iterateStateM :: (Applicative m) => GridState -> m GridState
-- iterateStateM = pure . iterateState 
