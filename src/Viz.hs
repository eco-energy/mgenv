{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Viz where

import Prelude hiding (zip, zipWith)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Physics.Storage
import Physics.PV
import Physics.Consumption
import Physics.Transmission

import HH
import Grid

import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Labelled (Graph)
import Data.Typeable (Typeable)
import Diagrams.TwoD.Text (Text)
import Data.Key

newtype GridViz = GridViz (Graph (TransmissionSpec, TransmissionState) (HHSpec, HHState))


batteryD :: BatterySpec -> BatteryState -> Diagram B
batteryD spec@BatterySpec{..} state@BatteryState{..} = circle 1 <> showSpecState spec state

panelD :: PVSpec -> Diagram B
panelD spec@PVSpec{..} = circle 1 <> showText spec

loadD :: Load -> LoadState -> Diagram B
loadD spec@Load{..} state@LoadState{..} = circle 1 <> showSpecState spec state


householdD :: HHSpec -> HHState -> Diagram B
householdD spec@HHSpec{..} (HHState state) = circle 1 <> showSpecState spec state

transmissionD :: TransmissionSpec -> TransmissionState -> Diagram B
transmissionD spec@TransmissionSpec{..} state@TransmissionState{..} = showSpecState spec state 


showSpecState :: (Typeable n, RealFloat n, Renderable (Text n) b, Show a1, Show a2) => a1 -> a2 -> QDiagram b V2 n Any
showSpecState spec state = showText spec <> showText state


showText :: (Typeable n, RealFloat n, Renderable (Text n) b, Show a) => a -> QDiagram b V2 n Any
showText = text . show

  
joinTHH :: (TransmissionSpec, TransmissionState) -> Diagram B -> Diagram B -> Diagram B
joinTHH tx h h' = (uncurry transmissionD tx) <> h <> h'


gridD :: GridViz -> Diagram B
gridD (GridViz g) = G.foldg mempty (uncurry householdD) joinTHH g


mkGridViz :: SampledGrid -> GridState -> GridViz
mkGridViz (SampledGrid spec) (GridState state) = GridViz $ combine
  where
    combine = G.edges $ (\((tsp, hsp, hsp'), (tst, hst, hst')) -> ((tsp, tst), (hsp, hst), (hsp', hst'))) <$> (zip espec estate)
    espec :: [(TransmissionSpec, HHSpec, HHSpec)]
    espec = G.edgeList spec
    estate :: [(TransmissionState, HHState, HHState)]
    estate = G.edgeList state
    
    
    
