{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Physics.Units
  (R, Q, V, Amp, Ohm, OhmMeters, MetersSq,
   W, Watts,
   AmpH, WH, WattHours,
   WattsPerMeter, WattsPerMeterSq,
   SoC, Efficiency,
   Sec, DelT,
   Meters,
   GeoC, EuclideanC, Temperature, MetersPerSecond, BearingDeg,
   ZonedTime, unZonedTime, incrementTime, mkZonedTime, dateStartToUTC, fromUTC,
   location, haversine, reverseHaversine, bearing, toRadians, toDegrees) where

import Data.Time.Clock.POSIX.Compat (posixSecondsToUTCTime)
import Streamly.Internal.Data.Time.Units
import qualified Data.Astro.Coordinate as A
import qualified Data.Astro.Types      as A
import qualified Data.Time             as T
import Data.Fixed

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


-- We want to evaluate our functions every second.
-- The time determines the generator stream.
-- The Storage and Transmission streams are integratable over time
-- The Consumption Stream is Rate Independent of the rest of the system?

type Sec = RelTime
type DelT = RelTime


newtype ZonedTime = ZonedTime { unZonedTime :: T.ZonedTime }

instance Eq ZonedTime where
  a == b = T.zonedTimeToUTC (unZonedTime a) ==  T.zonedTimeToUTC (unZonedTime b)

instance Ord ZonedTime where
  a `compare` b = T.zonedTimeToUTC (unZonedTime a) `compare` T.zonedTimeToUTC (unZonedTime b)

instance Show ZonedTime where
  show (ZonedTime a) = show a

tz :: T.TimeZone
tz = T.TimeZone
  { T.timeZoneMinutes = (5*60)
  , T.timeZoneSummerOnly = False
  , T.timeZoneName = ("PKT" :: String)
  }

toUTC :: ZonedTime -> T.UTCTime
toUTC = T.zonedTimeToUTC . unZonedTime

fromUTC :: T.UTCTime -> ZonedTime
fromUTC = ZonedTime . (T.utcToZonedTime tz)



incrementTime :: T.NominalDiffTime -> ZonedTime -> ZonedTime
incrementTime rate p = fromUTC (T.addUTCTime rate (toUTC p))  

mkZonedTime :: Integer -> Int -> Int -> ZonedTime
mkZonedTime y m d = fromUTC $ dateStartToUTC (T.fromGregorian y m d)

dateStartToUTC :: T.Day -> T.UTCTime
dateStartToUTC d = T.UTCTime {T.utctDay=d, T.utctDayTime= (0::T.DiffTime)}

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

type BearingDeg = R -- ANGLE IN Degrees


toRadians :: (RealFrac a, Floating a) => a -> a
toRadians d = (d `mod'` 360) * pi / 180

toDegrees :: (RealFrac a, Floating a) => a -> a
toDegrees r = mod' ((r * 180 / pi) + 360) 360.0

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

bearing :: GeoC -> GeoC -> BearingDeg
bearing (A.GeoC (A.DD lat0) (A.DD long0)) (A.GeoC (A.DD lat1) (A.DD long1)) = toDegrees $ atan2 y x
  where
    y = (sin (long' - long)) * cos (lat')
    x = (cos lat * sin lat') - (sin lat * cos lat' * cos (lat' - lat))
    [lat, long, lat', long'] = toDegrees <$> [lat0, long0, lat1, long1]
    

reverseHaversine :: GeoC -> Meters -> BearingDeg -> GeoC
reverseHaversine (A.GeoC (A.DD lat0) (A.DD long0)) d theta' = location (toDegrees lat') (toDegrees long')
  where
    lat' =  asin ((sin lat * cos angDist) + (cos lat * sin angDist * cos theta))
    long' = long - (atan2 (sin theta * sin angDist * cos lat) ((cos angDist) - (sin lat * sin lat')))
    angDist = d / earthRad
    theta = toRadians theta'
    lat = toRadians lat0
    long = toRadians long0
