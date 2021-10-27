{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Physics.PV where --(runPV, samplePVSpec, PVSpec) where

import Data.Time (ZonedTime, utctDay, zonedTimeToUTC)
import Data.Astro.Time.JulianDate
import Data.Astro.Time.GregorianCalendar (dayNumber)
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Sun
import Data.Astro.CelestialObject.RiseSet (RiseSetMB)
import Data.Astro.Time.Conv
import GHC.Generics (Generic)
import Physics.Units (R, WattsPerMeterSq, Temperature, Watts, Amp, V, Ohm, MetersPerSecond)
import Physics.Time
import Control.Monad.Bayes.Class
import Control.Monad (liftM)

import Prob.Randomizable
import qualified Streamly.Internal.Data.Unfold as UF

-- An implementation of the PVWatts Model.
-- Should be replaced by DeSotto's when we get the datasheets

data Mount = OpenRack
  | CloseRoofMount
  | InsulatedBack
  | Tracker
  deriving (Eq, Ord, Show, Generic)

data ModuleType = GlassCellGlass
  | GlassCellPolymerSheet
  | PolymerThinFilmSteel
  | LinearConcentrator
  deriving (Eq, Ord, Show, Generic)

-- | https://pvpmc.sandia.gov/modeling-steps/2-dc-module-iv/module-temperature/sandia-module-temperature-model/
moduleTemp :: Mount -> ModuleType -> WattsPerMeterSq -> Temperature -> R -> Temperature
moduleTemp !mount !modT !irradiance !ambientTemp !ws = irradiance * (exp (a + b + ws)) + ambientTemp
  where
    (a, b) = p modT mount
    p :: ModuleType -> Mount -> (R, R)
    p (GlassCellGlass) (OpenRack) = (-3.47, -0.0594)
    p (GlassCellGlass) (CloseRoofMount) = (-2.98, -0.0471)
    p (GlassCellPolymerSheet) (OpenRack) = (-3.56, -0.0750)
    p (GlassCellPolymerSheet) (InsulatedBack) = (-2.81, -0.0455)
    p (PolymerThinFilmSteel) (OpenRack) = (-3.58, -0.113)
    p (LinearConcentrator) (Tracker) = (-3.23, -0.130)
    p _ _ = error "Not mount/module type pair"

-- | https://pvpmc.sandia.gov/modeling-steps/2-dc-module-iv/cell-temperature/sandia-cell-temperature-model/ 
cellTemp :: Temperature -> WattsPerMeterSq -> Temperature
cellTemp !tMod !ePOA = tMod + (ePOA / eRef) * delT
  where
    eRef = 1000 :: WattsPerMeterSq
    delT = 10 -- bad assumption


horizonCoordinates :: GeographicCoordinates -> JulianDate -> HorizonCoordinates
horizonCoordinates loc jd = ec1ToHC loc jd (sunPosition2 jd)

effectiveIrradiance :: GeographicCoordinates -> PVSpec -> ZonedTime -> WattsPerMeterSq
effectiveIrradiance !loc PVSpec{..} !t = directNormalIrradiance * cos aoi
  where
    directNormalIrradiance :: WattsPerMeterSq
    directNormalIrradiance
      | (isDaytime solarAltitude) = flux * exp (-1 * opticalDepth * airMassRatio)
      | otherwise = 0
    isDaytime :: Double -> Bool
    isDaytime a
      | a > 0 = True
      | otherwise = False
    flux = 1160 + (75 * sin (2 * pi / 365 * (fromIntegral dayOfYear - 275)))
    opticalDepth = 0.174 + (0.035 * sin (2 * pi / 365 * (fromIntegral dayOfYear - 100)))
    airMassRatio = 1 / sin (toRadians $ hAltitude horizon)
    julianDay = lctUniversalTime . zonedTimeToLCT
    dayOfYear = (dayNumber . utctDay . zonedTimeToUTC) t
    horizon = horizonCoordinates loc (julianDay t)
    aoi = acos ((cos solarZenith * cos solarAzimuth) + (sin arrTilt * cos ((arrAzimuth - solarAzimuth))))
    (DD solarAltitude) = hAltitude horizon
    solarZenith = 90 - solarAltitude
    (DD solarAzimuth) = hAzimuth horizon

maxPowerPoint :: Watts -> WattsPerMeterSq -> Temperature -> R -> Watts
maxPowerPoint !modPower !effIrr !cellTemperature !tempCorrection
  | (effIrr > 125) = (effIrr / refIrr) * modPower * (1 + tempCorrection * (cellTemperature - refTemp))
  | otherwise = ((0.008 * effIrr**2) / refIrr) * modPower * (1 + tempCorrection * (cellTemperature - refTemp))
  where
    refIrr = 1000 :: WattsPerMeterSq
    refTemp = 25 :: Temperature


data PVSpec = PVSpec
  { arrAzimuth :: !R -- same as loc azimuth?
  , arrTilt    :: !R -- tilt of frame
  , tempCorrection :: !R
  , power :: !Watts
  , mount :: !Mount
  , moduleType :: !ModuleType
  } deriving (Eq, Ord, Show, Generic)


instance Randomizable PVSpec where
  sampleThis = samplePVSpec

samplePVSpec :: (MonadSample m) => m PVSpec
samplePVSpec = do
  arrAz <- do return 180 -- assume southward facing
  arrTilt <- normal 30 10
  tempCorrection <- liftM abs $ normal 0.0044 0.05
  power <- uniformD [50,100.. 500]
  mount <- do return OpenRack
  moduleType <- do return GlassCellGlass
  return $ PVSpec arrAz arrTilt tempCorrection power mount moduleType
{-# INLINE samplePVSpec #-}

-- How to use this:
-- effectiveIrradiance -> cellTemp -> maxPowerPoint
-- Which depends on the effective irradiance and the cellTemp

--type PVSeed = (GeographicCoordinates, PVSpec)

-- This should be used at the grid level
data EnvCond = EnvCond
  { windSpeed :: MetersPerSecond
  , ambientTemp :: Temperature
  } deriving (Eq, Ord, Show, Generic)

sampleEnvCond :: (MonadSample m) => m EnvCond
sampleEnvCond = do
  ws <- liftM abs $ normal 1 5
  aT <- normal 20 10
  return $ EnvCond ws aT

type EnvCondUF m = UF.Unfold m (GeographicCoordinates, ZonedTime) (EnvCond)


type PV m v i = UF.Unfold m (ZonedTime, Temperature, MetersPerSecond) (v, i)


type Irradiance m v i = UF.Unfold m (ZonedTime, Temperature, MetersPerSecond) (WattsPerMeterSq)

type ModuleTemp m = UF.Unfold m (Temperature, MetersPerSecond, WattsPerMeterSq) (Temperature)

type CellTemp m = UF.Unfold m (Temperature, WattsPerMeterSq) Temperature

type MaxPowerPoint m v i = UF.Unfold m (Temperature, WattsPerMeterSq) (v, i)


pvUF :: GeographicCoordinates -> PVSpec -> Irradiance m v i -> ModuleTemp m -> CellTemp m -> MaxPowerPoint m v i
pvUF = undefined

--pvUF :: GeographicCoordinates -> PVSpec -> PV m v i
--pvUF = 

runPV :: GeographicCoordinates -> PVSpec -> ZonedTime -> Temperature -> MetersPerSecond -> Watts
runPV loc spec time ambientTemp windSpeed = maxPowerPoint power effIrr cellT tempCorrection
  where
    effIrr = effectiveIrradiance loc spec time
    modT = moduleTemp mount moduleType effIrr ambientTemp windSpeed
    cellT = cellTemp modT effIrr
    (PVSpec {..}) = spec


{--
TODO: More granular array model

import qualified Data.NonEmpty as NonEmpty

data PVArray = Module PVModule
  | Serial (NonEmpty.T [] PVArray)
  | Parallel (NonEmpty.T [] PVArray)
  deriving (Eq, Ord, Show, Generic)

current :: [Watts] -> PVArray -> Amp
current (Serial a) = 
--powerOut String ps = 

--data PVArray = PVArray String Topology


--mkModules :: GenSpec -> PVArray
--mkModules GenSpec {..} =

data GenSpec = GenSpec
  { pvType :: PVModule
  , modInSeries :: Int
  , seriesInParallel :: Int
  } deriving (Eq, Ord, Show)
--}
