{-# LANGUAGE DeriveGeneric #-}
module Physics.Generation where

import Physics.Solar
import Physics.Units (R, Watts)
import GHC.Generics (Generic)

type WattsPerMeterSq = R
type Meter = R

data PanelSpec = PanelSpec
  { solarConvEff :: (WattsPerMeterSq)
  , height :: Meter
  , width :: Meter
  } deriving (Eq, Show)

panel :: PanelSpec -> (Watts -> a)
panel p = undefined

--data PanelArray a = Serial a | Parallel a deriving (Eq, Ord, Show, Generic)

-- Current and Voltage Monoids, Power and Energy Functors (dissipative elements don't )


data PanelArray = Serial | Parallel | PanelArray deriving (Eq, Ord, Show, Generic)

-- infix 9 -->

-- a (-->) b = Serial a b

-- infix 9 (|||)

-- a (|||) b = Parallel a b
