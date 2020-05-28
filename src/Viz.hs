{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Viz where

import Prelude hiding (zip, zipWith)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Physics.Storage
import Physics.Generation
import Physics.Consumption
import Physics.Transmission

import HH hiding (loc)
import Grid

import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Labelled (Graph)
import Data.Typeable (Typeable)
import Diagrams.TwoD.Text (Text)
import Data.Key

newtype GridViz = GridViz (Graph (TransmissionSpec, TransmissionState) (HHSpec, HHState)) deriving (Eq, Ord, Show)

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

generationD :: GenSpec -> Diagram B
generationD (GenSpec spec) = circle 4 # lc yellow <> showText spec

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

transmissionD :: TransmissionSpec -> TransmissionState -> Located (Diagram B) -> Located (Diagram B) -> Located (Diagram B)
transmissionD spec@TransmissionSpec{..} state@TransmissionState{..} h h' = showSpecState spec state # connectOutside (topName h) (topName h') `at` (loc h)
  where
    topName = fst . head . names . unLoc -- showSpecState spec state 

showSpecState :: (Show a1, Show a2) => a1 -> a2 -> Diagram B
showSpecState spec state = (showText spec <> circle 3) ||| (showText state <> circle 3) 


showText :: (Typeable n, RealFloat n, Renderable (Text n) b, Show a) => a -> QDiagram b V2 n Any
showText = text . show

  
joinTHH :: (TransmissionSpec, TransmissionState) -> Located (Diagram B) -> Located (Diagram B) -> Located (Diagram B)
joinTHH tx h h' = (uncurry transmissionD tx) h h'


gridD :: (Double, Double) -> GridViz -> Located (Diagram B)
gridD cp (GridViz g) = (G.foldg (circle 0 # named (GV "top") `at` p2 cp) (uncurry householdD) joinTHH g) 


mkGridViz :: SampledGrid -> GridState -> GridViz
mkGridViz (SampledGrid spec) (GridState state) = GridViz $ combine
  where
    combine = G.edges $ (\((tsp, hsp, hsp'), (tst, hst, hst')) -> ((tsp, tst), (hsp, hst), (hsp', hst'))) <$> (zip espec estate)
    espec :: [(TransmissionSpec, HHSpec, HHSpec)]
    espec = G.edgeList spec
    estate :: [(TransmissionState, HHState, HHState)]
    estate = G.edgeList state
    
    
    
