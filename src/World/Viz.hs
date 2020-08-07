{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DeriveDataTypeable #-}


module World.Viz where

import Prelude hiding (zip, zipWith)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Physics.Storage
import Physics.PV
import Physics.Consumption
import Physics.Transmission

import HH hiding (loc)
import Grid

import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Labelled (Graph)

import Data.Typeable (Typeable)
import Diagrams.TwoD.Text (Text)
import Data.Key

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

type TxE = (TransmissionSpec, TransmissionState)
type HHV = (HHSpec, HHState)

newtype GridViz = GridViz (Graph TxE HHV) deriving (Eq, Ord, Show)

data CompNames = BatteryD NodeId
  | NodeD NodeId
  | GenD NodeId
  | StorageD NodeId
  | ConsumptionD NodeId
  | HHD NodeId
  | THH (NodeId, NodeId)
  | GV String
  deriving (Typeable, Eq, Ord, Show)

instance IsName CompNames

batteryD :: BatterySpec -> BatteryState -> Diagram B
batteryD spec@BatterySpec{..} state@BatteryState{..} = (circle 4 # lc green <> showSpecState spec state)

generationD :: PVSpec -> Diagram B
generationD spec = circle 4 # lc yellow <> showText spec

loadD :: Load -> LoadState -> Diagram B
loadD spec@Load{..} state@LoadState{..} = circle 4 # lc red <> showSpecState spec state

consumptionD :: ConsumptionSpec -> ConsumptionState -> Diagram B
consumptionD (ConsumptionSpec specs) (ConsumptionState states) =  foldr (<>) mempty $ (uncurry loadD) <$> zip specs states

householdD :: HHSpec -> HHState -> Located (Diagram B)
householdD spec@HHSpec{..} (HHState (batState, conState)) =
  (circle 18 # lc blanchedalmond `atop` comps) # named (NodeD nId) `at` (p2 gridLoc)  
  where
    comps = (generationD generation) # named (GenD nId) 
      ||| (batteryD storage batState) # named (StorageD nId)
      ||| (consumptionD consumption conState) # named (ConsumptionD nId)

transmissionD :: TransmissionSpec -> TransmissionState -> Diagram B
transmissionD spec@TransmissionSpec{..} state@TransmissionState{..} = showSpecState spec state


showSpecState :: (Show a1, Show a2) => a1 -> a2 -> Diagram B
showSpecState spec state = circle 20 <> ((showText spec <> square 10) ||| (showText state <> square 10)) 


showText :: (Typeable n, RealFloat n, Renderable (Text n) b, Show a) => a -> QDiagram b V2 n Any
showText = text . show

  
joinTHH :: (TransmissionSpec, TransmissionState) -> Located (Diagram B) -> Located (Diagram B) -> Located (Diagram B)
joinTHH tx h h'
  | snd tx == mempty = mempty `at` (loc h)
  | otherwise = (uncurry transmissionD tx) # connectOutside
                  (topName . ns' $ h)
                  (topName . ns' $ h') `at` (loc h)
  where
    ns' = names . unLoc
    topName (x:_) = fst x -- showSpecState spec state
    topName [] = error "no name in diagram"


gridD :: (Double, Double) -> GridViz -> Located (Diagram B)
gridD cp (GridViz g) = (G.foldg (circle 0 # named (GV "top") `at` p2 cp) (uncurry householdD) joinTHH g) 


mkGridViz :: SampledGrid -> GridState -> GridViz
mkGridViz (SampledGrid spec) (GridState state) = GridViz $ G.edges combined
  where
    combined = (\((tsp, hsp, hsp'), (tst, hst, hst')) ->
                           ((tsp, tst), (hsp, hst), (hsp', hst')))
              <$> (zip espec estate)
    espec :: [(TransmissionSpec, HHSpec, HHSpec)]
    espec = G.edgeList spec
    estate :: [(TransmissionState, HHState, HHState)]
    estate = G.edgeList state
    


-- | A data type for specifying whether edges should be drawn on top
--   of vertices or vice versa.
data GraphLayering = EdgesOnTop | VerticesOnTop
  deriving (Show, Read, Eq, Ord)    

-- Decomposes a graph with a location into a map from the node to a location and a list of edges
vGraph :: Graph TxE HHV -> (M.Map HHV (P2 Double), [(HHV, P2 Double, HHV, P2 Double -> TxE, Path V2 Double)])
vGraph = undefined

-- | The same as 'drawGraph', but with an extra parameter allowing you
--   to specify whether vertices or edges should be drawn on top.
drawGraph'
  :: (Ord v, Semigroup m)
  => GraphLayering
  -> (v -> P2 Double -> QDiagram b V2 Double m)
  -> (v -> P2 Double -> v -> P2 Double -> e -> Path V2 Double -> QDiagram b V2 Double m)
  -> G.Graph e v
  -> QDiagram b V2 Double m
drawGraph' gl drawV drawE gr
  = case gl of
      EdgesOnTop    -> mconcat components
      VerticesOnTop -> mconcat (reverse components)
  where
    getGraph = undefined
    components =
      [ mconcat (map drawE' edges)
      , mconcat (map (uncurry drawV) (M.assocs vmap))
      ]
    (vmap, edges) = getGraph gr
    drawE' (v1,v2,e,p)
      = drawE v1 (fromJust $ M.lookup v1 vmap) v2 (fromJust $ M.lookup v2 vmap) e p

-- | Round-trip a graph through an external graphviz layout algorithm, and
