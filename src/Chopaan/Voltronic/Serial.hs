{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Voltronic.Serial
  ( -- * Device creation
    withVoltronicSerial
  , openVoltronicSerial
  , closeVoltronicSerial
    -- * Re-exports
  , VoltronicDev(..)
  , SerialConfig(..)
  , defaultSerialConfig
  ) where

import Chopaan.Voltronic
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Exception (bracket, IOException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO
import Control.Concurrent (threadDelay)

-- Note: In production, use 'serialport' or 'unix' package for proper RS232
-- This is a simplified implementation using file handles

-- | Open a Voltronic device on a serial port
openVoltronicSerial :: SerialConfig -> IO VoltronicDev
openVoltronicSerial cfg@SerialConfig{..} = do
  -- Open serial port
  -- In production: use System.Hardware.Serialport
  h <- openBinaryFile scPort ReadWriteMode
  hSetBuffering h NoBuffering

  -- Configure serial port settings via stty (Unix)
  -- In production, use proper serialport library
  let sttyCmd = "stty -F " ++ scPort ++
                " " ++ show scBaudRate ++
                " cs" ++ show scDataBits ++
                " -cstopb" ++  -- 1 stop bit
                " -parenb" ++  -- No parity
                " raw -echo"
  _ <- catch (callCommand sttyCmd) (\(_ :: IOException) -> pure ())

  pure VoltronicDev
    { vdConfig = cfg
    , vdSend   = BS.hPut h
    , vdRecv   = \n -> BS.hGet h n `catch` \(_ :: IOException) -> pure BS.empty
    , vdClose  = hClose h
    }
  where
    callCommand :: String -> IO ()
    callCommand cmd = do
      _ <- System.IO.readProcess' cmd
      pure ()

    -- Placeholder for System.Process.readProcess
    readProcess' :: String -> IO String
    readProcess' _ = pure ""

-- | Close a Voltronic device
closeVoltronicSerial :: VoltronicDev -> IO ()
closeVoltronicSerial = vdClose

-- | Bracket-style resource management
withVoltronicSerial :: SerialConfig -> (VoltronicDev -> IO a) -> IO a
withVoltronicSerial cfg = bracket (openVoltronicSerial cfg) closeVoltronicSerial
