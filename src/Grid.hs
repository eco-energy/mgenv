{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Grid where

import GHC.Generics (Generic)
import Physics.Storage (mkBattery)
import Physics.Units (GeoC, R)
import Node
import Data.MemoTrie
import GHC.Generics (Generic)
import Node

type Theta = R
type Gamma = R


mkNode = Node

data BatterySpec = BatterySpec
  { tEnergy :: R
  , cEff :: R
  , dEff :: R
  } deriving (Eq, Show, Ord, Generic)

data LoadSpec = LoadSpec
  { lpower :: R } deriving (Eq, Show, Ord, Generic)

data PanelSpec = PanelSpec
  { conversionEff :: R  } deriving (Eq, Show, Ord, Generic) 

data TransmissionSpec = TransmissionSpec
  { transmissionEfficiency :: R } deriving (Eq, Show, Ord, Generic)
 
type GridPoints = (R, Theta, Gamma)

type Loc = GeoC

data GridSpec = GridSpec
  { nNodes :: Int
  , loadS :: [LoadSpec]
  , panelS :: [PanelSpec]
  , batteryS :: [BatterySpec]
  , transmission :: [TransmissionSpec]
  , nodeDistanceMean :: R
  , nodeDistanceStd :: R
  } deriving (Eq, Show, Generic)




genNodes :: GridSpec -> [Node]
genNodes GridSpec {..} =  [mkNode $ i s c g | i <- range nNodes]
  where
    mkNode = Node
    s = mkBattery $ sample batteryS
    c = mkLoad $ sample loadS
    g = mkGen $ sample panelS
