{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Chopaan.Voltronic.Kibbutz
  ( -- * Kibbutz Integration
    VoltronicKibbutz(..)
  , VoltronicKibbutzConfig(..)
    -- * Lifecycle
  , runVoltronicKibbutz
  , initVoltronicKibbutz
    -- * Telemetry
  , TelemetryPoint(..)
  , telemetryToInflux
    -- * MQTT Bridge
  , MQTTConfig(..)
  , publishReading
  , subscribeCommands
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime, getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Chopaan.Voltronic
import Chopaan.Voltronic.Serial
import Chopaan.Voltronic.Node

--------------------------------------------------------------------------------
-- Kibbutz Configuration
--------------------------------------------------------------------------------

data VoltronicKibbutzConfig = VoltronicKibbutzConfig
  { vkcName        :: !Text                    -- ^ Kibbutz name (e.g., "Bismillah_Mor")
  , vkcInverters   :: ![InverterConfig]        -- ^ Inverter configurations
  , vkcMqtt        :: !(Maybe MQTTConfig)      -- ^ Optional MQTT publishing
  , vkcInfluxDb    :: !(Maybe InfluxConfig)    -- ^ Optional InfluxDB storage
  , vkcPollIntervalMs :: !Int                  -- ^ Global poll interval override
  } deriving (Eq, Show, Generic)

data MQTTConfig = MQTTConfig
  { mqttBroker   :: !Text
  , mqttPort     :: !Int
  , mqttUsername :: !(Maybe Text)
  , mqttPassword :: !(Maybe Text)
  , mqttBaseTopic :: !Text   -- ^ e.g., "chopaan/kibbutz/"
  } deriving (Eq, Show, Generic)

data InfluxConfig = InfluxConfig
  { influxHost     :: !Text
  , influxPort     :: !Int
  , influxDatabase :: !Text
  , influxUser     :: !(Maybe Text)
  , influxPassword :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Kibbutz State
--------------------------------------------------------------------------------

data VoltronicKibbutz = VoltronicKibbutz
  { vkConfig  :: !VoltronicKibbutzConfig
  , vkNodes   :: !(Map Text InverterNode)
  , vkState   :: !(TVar (Map Text InverterState))
  , vkCommands :: !(TQueue InverterCommand)
  }

--------------------------------------------------------------------------------
-- Telemetry
--------------------------------------------------------------------------------

data TelemetryPoint = TelemetryPoint
  { tpKibbutz     :: !Text
  , tpNodeId      :: !Text
  , tpTimestamp   :: !UTCTime
  , tpMeasurement :: !Text
  , tpValue       :: !Double
  , tpTags        :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

instance ToJSON TelemetryPoint
instance FromJSON TelemetryPoint

-- | Convert inverter state to InfluxDB line protocol
telemetryToInflux :: Text -> Text -> InverterState -> [Text]
telemetryToInflux kibbutz nodeId InverterState{..} =
  [ mkLine "grid_voltage"     isGridVoltage
  , mkLine "grid_frequency"   isGridFrequency
  , mkLine "output_voltage"   isOutputVoltage
  , mkLine "output_power"     (fromIntegral isOutputPower)
  , mkLine "load_percent"     (fromIntegral isLoadPercent)
  , mkLine "battery_voltage"  isBatteryVoltage
  , mkLine "battery_soc"      (fromIntegral isBatterySoc)
  , mkLine "battery_charge_a" (fromIntegral isBatteryChargeA)
  , mkLine "battery_discharge_a" (fromIntegral isBatteryDischargeA)
  , mkLine "pv_voltage"       isPvVoltage
  , mkLine "pv_current"       isPvCurrent
  , mkLine "pv_power"         isPvPower
  , mkLine "temperature"      (fromIntegral isTemperature)
  ]
  where
    mkLine :: Text -> Double -> Text
    mkLine field value = T.concat
      [ "inverter,kibbutz=", kibbutz, ",node=", nodeId, " "
      , field, "=", T.pack (show value), " "
      , T.pack (show $ utcToNanos isTimestamp)
      ]
    utcToNanos _ = 0 :: Integer  -- Placeholder

--------------------------------------------------------------------------------
-- MQTT Bridge
--------------------------------------------------------------------------------

-- | Publish a reading to MQTT
-- Topic: {baseTopic}/{kibbutz}/{nodeId}/state
publishReading :: MQTTConfig -> Text -> InverterReading -> IO ()
publishReading MQTTConfig{..} kibbutz InverterReading{..} = do
  -- Placeholder: In production, use Network.MQTT.Client
  let topic = T.concat [mqttBaseTopic, kibbutz, "/", irNodeId, "/state"]
      payload = encode irState
  putStrLn $ "MQTT Publish: " ++ T.unpack topic
  -- mqttPublish client topic payload

-- | Subscribe to command topic
-- Topic: {baseTopic}/{kibbutz}/{nodeId}/cmd
subscribeCommands :: MQTTConfig -> Text -> (Text -> InverterCommand -> IO ()) -> IO ()
subscribeCommands MQTTConfig{..} kibbutz handler = do
  -- Placeholder: In production, use Network.MQTT.Client
  let topic = T.concat [mqttBaseTopic, kibbutz, "/+/cmd"]
  putStrLn $ "MQTT Subscribe: " ++ T.unpack topic
  -- mqttSubscribe client topic (parseAndHandle handler)

instance ToJSON InverterState
instance FromJSON InverterState
instance ToJSON InverterReading
instance FromJSON InverterReading

--------------------------------------------------------------------------------
-- Kibbutz Lifecycle
--------------------------------------------------------------------------------

-- | Initialize a Voltronic kibbutz
initVoltronicKibbutz :: MonadIO m => VoltronicKibbutzConfig -> m (Either Text VoltronicKibbutz)
initVoltronicKibbutz cfg@VoltronicKibbutzConfig{..} = liftIO $ do
  -- Initialize all inverter nodes
  results <- forM vkcInverters $ \ic -> do
    node <- mkInverterNode ic
    pure (icNodeId ic, node)

  let (failures, successes) = partitionResults results

  if not (null failures)
    then pure $ Left $ T.concat
      [ "Failed to initialize nodes: "
      , T.intercalate ", " (map fst failures)
      ]
    else do
      stateVar <- newTVarIO Map.empty
      cmdQueue <- newTQueueIO
      pure $ Right VoltronicKibbutz
        { vkConfig = cfg
        , vkNodes = Map.fromList successes
        , vkState = stateVar
        , vkCommands = cmdQueue
        }
  where
    partitionResults :: [(Text, Either VoltronicError InverterNode)]
                     -> ([(Text, VoltronicError)], [(Text, InverterNode)])
    partitionResults = foldr go ([], [])
      where
        go (nid, Left err) (fs, ss) = ((nid, err):fs, ss)
        go (nid, Right n)  (fs, ss) = (fs, (nid, n):ss)

-- | Run the kibbutz - polls all inverters, publishes telemetry
runVoltronicKibbutz :: forall t m. (S.IsStream t, S.MonadAsync m)
                    => VoltronicKibbutz
                    -> t m InverterReading
runVoltronicKibbutz VoltronicKibbutz{..} =
  S.concatMapWith S.async nodeStream (S.fromList $ Map.elems vkNodes)
  where
    nodeStream :: InverterNode -> t m InverterReading
    nodeStream node = S.mapM process $ inverterStateStream node

    process :: InverterReading -> m InverterReading
    process reading@InverterReading{..} = do
      -- Update state
      liftIO $ atomically $ modifyTVar' vkState (Map.insert irNodeId irState)
      -- Publish to MQTT if configured
      liftIO $ case vkcMqtt vkConfig of
        Nothing -> pure ()
        Just mqtt -> publishReading mqtt (vkcName vkConfig) reading
      pure reading

--------------------------------------------------------------------------------
-- Example Configuration
--------------------------------------------------------------------------------

-- | Example: Inverex Nitrox deployment
exampleInverexConfig :: VoltronicKibbutzConfig
exampleInverexConfig = VoltronicKibbutzConfig
  { vkcName = "Bismillah_Mor_v2"
  , vkcInverters =
      [ InverterConfig
          { icNodeId = "inverex_01"
          , icSerial = defaultSerialConfig "/dev/ttyUSB0"
          , icPollMs = 5000
          , icInverterType = InverexNitrox
          }
      , InverterConfig
          { icNodeId = "inverex_02"
          , icSerial = defaultSerialConfig "/dev/ttyUSB1"
          , icPollMs = 5000
          , icInverterType = InverexNitrox
          }
      ]
  , vkcMqtt = Just MQTTConfig
      { mqttBroker = "mqtt.chopaan.local"
      , mqttPort = 1883
      , mqttUsername = Nothing
      , mqttPassword = Nothing
      , mqttBaseTopic = "chopaan/kibbutz/"
      }
  , vkcInfluxDb = Just InfluxConfig
      { influxHost = "localhost"
      , influxPort = 8086
      , influxDatabase = "chopaanMQTT"
      , influxUser = Nothing
      , influxPassword = Nothing
      }
  , vkcPollIntervalMs = 5000
  }
