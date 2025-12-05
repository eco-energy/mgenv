{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Voltronic.Types
  ( -- * Device Status
    DeviceMode(..)
  , parseDeviceMode
    -- * Battery Types
  , BatteryType(..)
    -- * Status Flags
  , StatusFlags(..)
  , parseStatusFlags
    -- * Fault Codes
  , FaultCode(..)
  , parseFaultCodes
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Device Mode (QMOD response)
--------------------------------------------------------------------------------

data DeviceMode
  = PowerOn           -- ^ 'P' - Power on mode
  | Standby           -- ^ 'S' - Standby mode
  | Line              -- ^ 'L' - Line mode (grid)
  | Battery           -- ^ 'B' - Battery mode
  | Fault             -- ^ 'F' - Fault mode
  | HybridLine        -- ^ 'H' - Hybrid mode (line)
  | HybridBattery     -- ^ 'Y' - Hybrid mode (battery)
  | UnknownMode Char
  deriving (Eq, Show, Generic)

instance ToJSON DeviceMode
instance FromJSON DeviceMode

parseDeviceMode :: Char -> DeviceMode
parseDeviceMode 'P' = PowerOn
parseDeviceMode 'S' = Standby
parseDeviceMode 'L' = Line
parseDeviceMode 'B' = Battery
parseDeviceMode 'F' = Fault
parseDeviceMode 'H' = HybridLine
parseDeviceMode 'Y' = HybridBattery
parseDeviceMode c   = UnknownMode c

--------------------------------------------------------------------------------
-- Battery Types (PBT command)
--------------------------------------------------------------------------------

data BatteryType
  = AGM           -- ^ 0
  | Flooded       -- ^ 1
  | UserDefined   -- ^ 2
  | Pylontech     -- ^ 3
  | Shinheung     -- ^ 4
  | WECO          -- ^ 5
  | Soltaro       -- ^ 6
  | LIBattery     -- ^ 7 - Generic lithium
  | LIC           -- ^ 8 - LIC type
  deriving (Eq, Show, Generic, Enum)

instance ToJSON BatteryType
instance FromJSON BatteryType

--------------------------------------------------------------------------------
-- Status Flags (from QPIGS)
--------------------------------------------------------------------------------

data StatusFlags = StatusFlags
  { sfSbuPriority          :: !Bool  -- ^ bit 7: SBU priority version
  , sfConfigChanged        :: !Bool  -- ^ bit 6: Configuration changed
  , sfSccFirmwareUpdated   :: !Bool  -- ^ bit 5: SCC firmware updated
  , sfLoadOn               :: !Bool  -- ^ bit 4: Load on
  , sfBatteryVoltSteady    :: !Bool  -- ^ bit 3: Battery voltage to steady while charging
  , sfCharging             :: !Bool  -- ^ bit 2: Charging on
  , sfChargingScc          :: !Bool  -- ^ bit 1: Charging on SCC
  , sfChargingAc           :: !Bool  -- ^ bit 0: Charging on AC
  } deriving (Eq, Show, Generic)

instance ToJSON StatusFlags
instance FromJSON StatusFlags

-- | Parse 8-character binary status string (e.g., "10010110")
parseStatusFlags :: Text -> Maybe StatusFlags
parseStatusFlags t
  | T.length t < 8 = Nothing
  | otherwise = Just StatusFlags
      { sfSbuPriority        = t `bitAt` 0
      , sfConfigChanged      = t `bitAt` 1
      , sfSccFirmwareUpdated = t `bitAt` 2
      , sfLoadOn             = t `bitAt` 3
      , sfBatteryVoltSteady  = t `bitAt` 4
      , sfCharging           = t `bitAt` 5
      , sfChargingScc        = t `bitAt` 6
      , sfChargingAc         = t `bitAt` 7
      }
  where
    bitAt s i = T.index s i == '1'

--------------------------------------------------------------------------------
-- Fault Codes (from QPIWS)
--------------------------------------------------------------------------------

data FaultCode
  = NoFault
  | FanLocked
  | InverterOverTemp
  | BatteryOverVoltage
  | BatteryUnderVoltage
  | OutputShortCircuit
  | InverterOverVoltage
  | OutputOverload
  | BusOverVoltage
  | BusSoftStartFail
  | PvOverCurrent
  | PvOverVoltage
  | DcDcOverCurrent
  | BatteryDisconnect
  | CurrentSensorFail
  | BatteryShort
  | PowerLimitWarning
  | PvVoltageHigh
  | MpptOverloadFault
  | MpptOverloadWarning
  | BatteryLowAlarm
  | BatteryUnderShutdown
  | OverTemp
  | UnknownFault Int
  deriving (Eq, Show, Generic)

instance ToJSON FaultCode
instance FromJSON FaultCode

-- | Parse QPIWS warning status (32-bit binary string)
parseFaultCodes :: Text -> [FaultCode]
parseFaultCodes t = foldr checkBit [] (zip [0..] (T.unpack t))
  where
    checkBit (i, '1') acc = bitToFault i : acc
    checkBit _        acc = acc

    bitToFault :: Int -> FaultCode
    bitToFault 1  = InverterOverTemp
    bitToFault 2  = BusOverVoltage
    bitToFault 3  = BusSoftStartFail
    bitToFault 4  = PvOverCurrent
    bitToFault 5  = PvOverVoltage
    bitToFault 6  = DcDcOverCurrent
    bitToFault 7  = BatteryOverVoltage
    bitToFault 8  = BatteryUnderVoltage
    bitToFault 9  = OutputOverload
    bitToFault 10 = OutputShortCircuit
    bitToFault 11 = InverterOverVoltage
    bitToFault 12 = FanLocked
    bitToFault n  = UnknownFault n
