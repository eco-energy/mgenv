{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Chopaan.Voltronic.Node
  ( -- * Inverter Node
    InverterNode(..)
  , InverterConfig(..)
  , InverterState(..)
  , InverterReading(..)
    -- * Node operations
  , mkInverterNode
  , pollInverter
  , setOutputPriority
  , setChargingPriority
    -- * Streaming
  , inverterStream
  , inverterStateStream
    -- * Integration types
  , InverterCommand(..)
  , OutputPriority(..)
  , ChargingPriority(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.ByteString (ByteString)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Chopaan.Voltronic
import Chopaan.Voltronic.Serial

--------------------------------------------------------------------------------
-- Node Types
--------------------------------------------------------------------------------

data InverterConfig = InverterConfig
  { icNodeId     :: !Text           -- ^ Unique identifier
  , icSerial     :: !SerialConfig   -- ^ Serial port config
  , icPollMs     :: !Int            -- ^ Polling interval (ms)
  , icInverterType :: !InverterType
  } deriving (Eq, Show, Generic)

data InverterType
  = Axpert
  | InfiniSolar
  | InverexNitrox
  | InverexVeyron
  | VoltronicGeneric
  deriving (Eq, Show, Generic)

data InverterNode = InverterNode
  { inConfig :: !InverterConfig
  , inDev    :: !VoltronicDev
  }

data InverterState = InverterState
  { isTimestamp       :: !UTCTime
  , isGridVoltage     :: !Double
  , isGridFrequency   :: !Double
  , isOutputVoltage   :: !Double
  , isOutputPower     :: !Int
  , isLoadPercent     :: !Int
  , isBatteryVoltage  :: !Double
  , isBatterySoc      :: !Int
  , isBatteryChargeA  :: !Int
  , isBatteryDischargeA :: !Int
  , isPvVoltage       :: !Double
  , isPvCurrent       :: !Double
  , isPvPower         :: !Double
  , isTemperature     :: !Int
  , isOnline          :: !Bool
  , isRawFlags        :: !Text
  } deriving (Eq, Show, Generic)

data InverterReading = InverterReading
  { irNodeId :: !Text
  , irState  :: !InverterState
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Priority Types
--------------------------------------------------------------------------------

data OutputPriority
  = UtilityFirst    -- ^ Grid first, then battery
  | SolarFirst      -- ^ Solar first, then grid
  | SBU             -- ^ Solar -> Battery -> Utility
  deriving (Eq, Show, Generic, Enum)

data ChargingPriority
  = ChargeUtilityFirst   -- ^ Charge from grid first
  | ChargeSolarFirst     -- ^ Charge from solar first
  | ChargeSolarUtility   -- ^ Charge from both
  | ChargeSolarOnly      -- ^ Only charge from solar
  deriving (Eq, Show, Generic, Enum)

data InverterCommand
  = SetOutputPriority OutputPriority
  | SetChargingPriority ChargingPriority
  | SetBatteryType Int
  | CustomCommand ByteString
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Node Operations
--------------------------------------------------------------------------------

-- | Create an inverter node
mkInverterNode :: MonadIO m => InverterConfig -> m (Either VoltronicError InverterNode)
mkInverterNode cfg@InverterConfig{..} = liftIO $ do
  dev <- openVoltronicSerial icSerial
  -- Test connection with QID
  resp <- execCommand dev qid
  case resp of
    Left err -> do
      closeVoltronicSerial dev
      pure $ Left err
    Right _ -> pure $ Right InverterNode
      { inConfig = cfg
      , inDev = dev
      }

-- | Poll inverter for current state
pollInverter :: MonadIO m => InverterNode -> m (Either VoltronicError InverterState)
pollInverter InverterNode{..} = do
  now <- liftIO getCurrentTime
  resp <- execCommandRaw inDev qpigs
  pure $ case resp of
    Left err -> Left err
    Right bs -> case parseQPIGS bs of
      Left err -> Left err
      Right QPIGSData{..} -> Right InverterState
        { isTimestamp        = now
        , isGridVoltage      = qGridVoltage
        , isGridFrequency    = qGridFrequency
        , isOutputVoltage    = qOutputVoltage
        , isOutputPower      = qOutputActivePwr
        , isLoadPercent      = qOutputLoadPct
        , isBatteryVoltage   = qBatteryVoltage
        , isBatterySoc       = qBatteryCapacity
        , isBatteryChargeA   = qBatteryChargeCur
        , isBatteryDischargeA = qBatteryDischargeCur
        , isPvVoltage        = qPvInputVoltage
        , isPvCurrent        = qPvInputCurrent
        , isPvPower          = qPvInputVoltage * qPvInputCurrent
        , isTemperature      = qHeatSinkTemp
        , isOnline           = True
        , isRawFlags         = qStatusFlags
        }

-- | Set output source priority
setOutputPriority :: MonadIO m => InverterNode -> OutputPriority -> m (Either VoltronicError ())
setOutputPriority InverterNode{..} prio = do
  resp <- execCommand inDev (pop $ fromEnum prio)
  pure $ case resp of
    Left err -> Left err
    Right (Response bs) ->
      if bs == "ACK"
        then Right ()
        else Left DeviceNak

-- | Set charging source priority
setChargingPriority :: MonadIO m => InverterNode -> ChargingPriority -> m (Either VoltronicError ())
setChargingPriority InverterNode{..} prio = do
  resp <- execCommand inDev (pcp $ fromEnum prio)
  pure $ case resp of
    Left err -> Left err
    Right (Response bs) ->
      if bs == "ACK"
        then Right ()
        else Left DeviceNak

--------------------------------------------------------------------------------
-- Streaming Interface
--------------------------------------------------------------------------------

-- | Stream of inverter readings at configured poll interval
inverterStream :: (S.IsStream t, S.MonadAsync m)
               => InverterNode
               -> t m (Either VoltronicError InverterReading)
inverterStream node@InverterNode{..} =
  S.repeatM poll
  where
    poll = do
      liftIO $ threadDelay (icPollMs inConfig * 1000)
      state <- pollInverter node
      pure $ InverterReading (icNodeId inConfig) <$> state

-- | Stream only successful states, with offline marker on errors
inverterStateStream :: (S.IsStream t, S.MonadAsync m)
                    => InverterNode
                    -> t m InverterReading
inverterStateStream node@InverterNode{..} =
  S.mapMaybe id $ S.map toReading $ inverterStream node
  where
    toReading (Right r) = Just r
    toReading (Left _)  = Nothing  -- Could emit offline state instead

--------------------------------------------------------------------------------
-- Multi-Node Fleet
--------------------------------------------------------------------------------

-- | Poll multiple inverters concurrently
pollFleet :: (S.IsStream t, S.MonadAsync m)
          => [InverterNode]
          -> t m InverterReading
pollFleet nodes =
  S.concatMapWith S.async inverterStateStream (S.fromList nodes)

-- | Execute command on all nodes
broadcastCommand :: MonadIO m
                 => [InverterNode]
                 -> InverterCommand
                 -> m [(Text, Either VoltronicError ())]
broadcastCommand nodes cmd = mapM execCmd nodes
  where
    execCmd node = do
      result <- case cmd of
        SetOutputPriority p   -> setOutputPriority node p
        SetChargingPriority p -> setChargingPriority node p
        SetBatteryType t      -> do
          resp <- execCommand (inDev node) (pbt t)
          pure $ either Left (const $ Right ()) resp
        CustomCommand bs      -> do
          resp <- execCommand (inDev node) (Command bs)
          pure $ either Left (const $ Right ()) resp
      pure (icNodeId $ inConfig node, result)
