module Env.Demand where

import RIO
import RIO.Time


afterDusk :: ZonedTime -> SunSet -> Boolean

afterSunrise :: ZonedTime -> Sunrise -> Boolean

officeHours :: ZonedTime -> Boolean

type Watts = Double

data Load = Load
  { power :: Watts
  , light :: Bool
  , cooling :: Bool
  , continuous :: Bool
  } deriving (Eq, Show)

type Line = ((Num m x b y) => x -> m -> b -> y)

type Demand m x b = (Line -> R)

gradient, offset :: R

data Determinants = Determinants

getDeterminants :: ZonedTime -> Determinants

demandEvent :: LeadTime -> [Load] -> (Watts, ZonedTime)
demandEvent time officeHours sleepTime isDomestic ls =
  where    
    cloads = filter continuous ls
