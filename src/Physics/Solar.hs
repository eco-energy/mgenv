module Physics.Solar where

import RIO.Time
import Data.Astro.Time.JulianDate
import Data.Astro.Time.GregorianCalendar (dayNumber)
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Sun
import Data.Astro.CelestialObject.RiseSet (RiseSetMB)
import Data.Astro.Time.Conv

import Physics.Units (WattsPerMeterSq)


lct :: LocalCivilTime
lct = lctFromYMDHMS (DH 1) 2019 10 11 3 21 0

horizonCoordinates :: GeographicCoordinates -> JulianDate -> HorizonCoordinates
horizonCoordinates loc jd = ec1ToHC loc jd (sunPosition2 jd)


setupDay :: GeographicCoordinates -> ZonedTime -> RiseSetMB
setupDay loc day = sunRiseAndSet loc verticalShift lcd
  where
    lcd = zonedTimeToLCD day
    verticalShift = 0.833333


directRadiation :: GeographicCoordinates -> ZonedTime -> WattsPerMeterSq
directRadiation loc t
  | (isDaytime alt) = flux * exp (-1 * opticalDepth * airMassRatio)
  | otherwise = 0
  where
    isDaytime :: DecimalDegrees -> Bool
    isDaytime (DD a)
      | a > 0 = True
      | otherwise = False
    flux = 1160 + (75 * sin (2 * pi / 365 * (fromIntegral dayOfYear - 275)))
    opticalDepth = 0.174 + (0.035 * sin (2 * pi / 365 * (fromIntegral dayOfYear - 100)))
    airMassRatio = 1 / sin (toRadians alt)
    alt = hAltitude horizon
    jd = lctUniversalTime . zonedTimeToLCT
    dayOfYear = (dayNumber . utctDay . zonedTimeToUTC) t
    horizon = horizonCoordinates loc (jd t)
