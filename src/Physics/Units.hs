{-# LANGUAGE TypeSynonymInstances #-}
module Physics.Units
  (R, Q, V, Amp,
   W, Watts,
   AmpH, WH, WattHours,
   WattsPerMeter, WattsPerMeterSq,
   SoC, Efficiency,
   Sec, DelT,
   Meters,
   GeoC, EuclideanC,
   ZonedTime, unZonedTime,
   location) where


import qualified Data.Astro.Coordinate as A
import qualified Data.Astro.Types      as A
import qualified Data.Time             as T

-- Base Type
type R = Double


-- Physical Quantities
type Q = R
type V = R
type Amp = R


type W = R
type Watts = W

type AmpH = R

type WH = R
type WattHours = WH

type WattsPerMeterSq = R

type WattsPerMeter = R


-- Dimensionless Constants
type SoC = R
type Efficiency = R


-- Time
type Sec = Int
type DelT = Sec

newtype ZonedTime = ZonedTime { unZonedTime :: T.ZonedTime }

instance Eq ZonedTime where
  a == b = T.zonedTimeToUTC (unZonedTime a) ==  T.zonedTimeToUTC (unZonedTime b)

instance Show ZonedTime where
  show (ZonedTime a) = show a

-- Distance
type Meters = R

type GeoC = A.GeographicCoordinates

type EuclideanC = (R, R, R)


location :: Double -> Double -> GeoC
location lat long = A.GeoC (A.DD lat) (A.DD long)

