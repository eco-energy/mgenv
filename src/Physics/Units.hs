{-# LANGUAGE TypeSynonymInstances #-}
module Physics.Units
  (R, Q, V, Amp, Ohm, OhmMeters, MetersSq,
   W, Watts,
   AmpH, WH, WattHours,
   WattsPerMeter, WattsPerMeterSq,
   SoC, Efficiency,
   Sec, DelT,
   Meters,
   GeoC, EuclideanC, Temperature, MetersPerSecond,
   ZonedTime, unZonedTime,
   location, haversine, reverseHaversine) where


import qualified Data.Astro.Coordinate as A
import qualified Data.Astro.Types      as A
import qualified Data.Time             as T

-- Base Type
type R = Double


-- Physical Quantities
type Q = R
type V = R
type Amp = R
type Ohm = R

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

type EuclideanC = (Meters, Meters)

type Temperature = R

location :: Double -> Double -> GeoC
location lat long = A.GeoC (A.DD lat) (A.DD long)

type MetersPerSecond = R

type MetersSq = R

type OhmMeters = R

type Theta = R -- ANGLE


toRadians :: Floating a => a -> a
toRadians d = d * pi / 180

toDegrees :: Floating a => a -> a
toDegrees r = r * 180 / pi

earthRad :: Meters
earthRad = (6378137 :: Meters)

haversine :: GeoC -> GeoC -> Meters
haversine c0 c1 = earthRad * c
  where
    a = square (sin dlat) + cosr lat0 * cosr lat1 * square (sin dlon)
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    dlat = toRadians (lat0 - lat1) / 2
    dlon = toRadians (long0 - long1) / 2
    cosr = cos . toRadians
    square x = x * x
    (A.GeoC (A.DD lat0) (A.DD long0)) = c0
    (A.GeoC (A.DD lat1) (A.DD long1)) = c1
  

reverseHaversine :: GeoC -> Meters -> Theta -> GeoC
reverseHaversine (A.GeoC (A.DD lat) (A.DD long)) d theta = location lat' long'
  where
    lat' = toDegrees $ asin ((sin lat * cos angDist) + (cos lat * sin angDist) + cos theta)
    long' = toDegrees $ long + (atan2 (sin theta * sin angDist * cos lat) ((cos angDist) - (sin lat * sin lat')))
    angDist = d / earthRad
