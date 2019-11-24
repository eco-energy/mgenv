{-# LANGUAGE DeriveGeneric #-}
module Physics.Transmission where

import Physics.Units
import GHC.Generics (Generic)

class Transmission a where
  thing :: a

data Bus = Bus
  { eff :: WattsPerMeter
  , length :: Meters
  } deriving (Eq, Show)

data EdgeState = EdgeState
  { dispatch :: (Bus -> (Watts, Sec))
  , recieve :: (Bus -> (Watts, Sec))
  , tLoss :: (Bus) -> (Watts, Sec)
  } deriving (Generic)

run = undefined
