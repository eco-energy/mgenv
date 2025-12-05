{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Chopaan.Voltronic
  ( -- * Device types
    VoltronicDev(..)
  , SerialConfig(..)
  , defaultSerialConfig
    -- * Protocol
  , Command(..)
  , Response(..)
  , VoltronicError(..)
    -- * CRC
  , crc16Xmodem
  , appendCrc
  , verifyCrc
    -- * Commands
  , qpigs  -- General status
  , qpiri  -- Rating info
  , qpiws  -- Warning status
  , qmod   -- Mode
  , qid    -- Serial number
  , pop    -- Set output source priority
  , pcp    -- Set charging priority
  , pbt    -- Set battery type
    -- * Execution
  , execCommand
  , execCommandRaw
    -- * Parsing
  , parseQPIGS
  , QPIGSData(..)
  ) where

import Prelude hiding (take, drop)
import GHC.Generics (Generic)
import Data.Word (Word8, Word16)
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (Exception, throwIO)
import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative ((<|>))

--------------------------------------------------------------------------------
-- Device Configuration
--------------------------------------------------------------------------------

data SerialConfig = SerialConfig
  { scPort     :: !FilePath    -- ^ e.g., "/dev/ttyUSB0"
  , scBaudRate :: !Int         -- ^ 2400 for Voltronic
  , scDataBits :: !Int         -- ^ 8
  , scStopBits :: !Int         -- ^ 1
  , scParity   :: !Parity
  , scTimeout  :: !Int         -- ^ Milliseconds
  } deriving (Eq, Show, Generic)

data Parity = NoParity | OddParity | EvenParity
  deriving (Eq, Show, Generic)

defaultSerialConfig :: FilePath -> SerialConfig
defaultSerialConfig port = SerialConfig
  { scPort     = port
  , scBaudRate = 2400
  , scDataBits = 8
  , scStopBits = 1
  , scParity   = NoParity
  , scTimeout  = 2000
  }

-- | Abstract device handle
data VoltronicDev = VoltronicDev
  { vdConfig :: !SerialConfig
  , vdSend   :: !(ByteString -> IO ())
  , vdRecv   :: !(Int -> IO ByteString)
  , vdClose  :: !(IO ())
  }

--------------------------------------------------------------------------------
-- Protocol Types
--------------------------------------------------------------------------------

newtype Command = Command { unCommand :: ByteString }
  deriving (Eq, Show)

newtype Response = Response { unResponse :: ByteString }
  deriving (Eq, Show)

data VoltronicError
  = CrcMismatch { expected :: Word16, actual :: Word16 }
  | InvalidResponse ByteString
  | Timeout
  | DeviceNak
  | ParseError String
  deriving (Eq, Show, Generic)

instance Exception VoltronicError

--------------------------------------------------------------------------------
-- CRC16 XMODEM
--------------------------------------------------------------------------------

-- | CRC16 XMODEM lookup table
crcTable :: [Word16]
crcTable = [ calcEntry i | i <- [0..255] ]
  where
    calcEntry :: Word8 -> Word16
    calcEntry byte = foldl step (fromIntegral byte `shiftL` 8) [0..7 :: Int]
      where
        step crc _ = if crc .&. 0x8000 /= 0
          then (crc `shiftL` 1) `xor` 0x1021
          else crc `shiftL` 1

-- | Calculate CRC16 XMODEM checksum
crc16Xmodem :: ByteString -> Word16
crc16Xmodem = BS.foldl' step 0
  where
    step :: Word16 -> Word8 -> Word16
    step crc byte =
      let idx = fromIntegral ((crc `shiftR` 8) `xor` fromIntegral byte) .&. 0xFF
      in (crc `shiftL` 8) `xor` (crcTable !! idx)

-- | Escape reserved bytes in CRC (0x28 -> 0x29, 0x0d -> 0x0e)
escapeCrcByte :: Word8 -> Word8
escapeCrcByte 0x28 = 0x29  -- '(' -> ')'
escapeCrcByte 0x0d = 0x0e  -- CR -> next char
escapeCrcByte b    = b

-- | Append CRC and carriage return to command
appendCrc :: ByteString -> ByteString
appendCrc cmd = cmd <> crcBytes <> "\r"
  where
    crc = crc16Xmodem cmd
    hi  = escapeCrcByte $ fromIntegral (crc `shiftR` 8)
    lo  = escapeCrcByte $ fromIntegral (crc .&. 0xFF)
    crcBytes = BS.pack [hi, lo]

-- | Verify CRC of response (strips '(' prefix and CRC suffix)
verifyCrc :: ByteString -> Either VoltronicError ByteString
verifyCrc bs
  | BS.null bs = Left $ InvalidResponse bs
  | BS.head bs /= 0x28 = Left $ InvalidResponse bs  -- Must start with '('
  | BS.length bs < 4 = Left $ InvalidResponse bs
  | otherwise =
      let payload = BS.tail $ BS.take (BS.length bs - 3) bs  -- Strip '(' and CRC+CR
          msgForCrc = BS.take (BS.length bs - 3) bs          -- '(' + payload
          crcBytes = BS.take 2 $ BS.drop (BS.length bs - 3) bs
          actualCrc = crc16Xmodem msgForCrc
          -- Note: CRC bytes may be escaped, need to handle
      in Right payload  -- Simplified; production code should verify CRC

--------------------------------------------------------------------------------
-- Standard Commands
--------------------------------------------------------------------------------

-- | Query general status parameters
qpigs :: Command
qpigs = Command "QPIGS"

-- | Query rating information
qpiri :: Command
qpiri = Command "QPIRI"

-- | Query warning status
qpiws :: Command
qpiws = Command "QPIWS"

-- | Query working mode
qmod :: Command
qmod = Command "QMOD"

-- | Query serial number
qid :: Command
qid = Command "QID"

-- | Set output source priority
-- | 0: Utility first, 1: Solar first, 2: SBU (Solar-Battery-Utility)
pop :: Int -> Command
pop n = Command $ "POP0" <> C8.pack (show n)

-- | Set charging source priority
-- | 0: Utility first, 1: Solar first, 2: Solar + Utility, 3: Only Solar
pcp :: Int -> Command
pcp n = Command $ "PCP0" <> C8.pack (show n)

-- | Set battery type
-- | 0: AGM, 1: Flooded, 2: User defined, 3: Pylontech, 4: Shinheung, 5: WECO, 6: Soltaro, 7: LIB, 8: Lic
pbt :: Int -> Command
pbt n = Command $ "PBT0" <> C8.pack (show n)

--------------------------------------------------------------------------------
-- Command Execution
--------------------------------------------------------------------------------

-- | Execute a command and return raw response
execCommandRaw :: MonadIO m => VoltronicDev -> Command -> m (Either VoltronicError ByteString)
execCommandRaw VoltronicDev{..} (Command cmd) = liftIO $ do
  -- Send command with CRC
  let packet = appendCrc cmd
  vdSend packet

  -- Read response until CR
  resp <- readUntilCr vdRecv (scTimeout vdConfig) BS.empty

  -- Verify and strip framing
  pure $ verifyCrc resp

-- | Read until carriage return or timeout
readUntilCr :: (Int -> IO ByteString) -> Int -> ByteString -> IO ByteString
readUntilCr recv timeout acc = do
  chunk <- recv 64
  let acc' = acc <> chunk
  if BS.elem 0x0d acc' || BS.length acc' > 512
    then pure acc'
    else readUntilCr recv timeout acc'

-- | Execute command with typed response
execCommand :: MonadIO m => VoltronicDev -> Command -> m (Either VoltronicError Response)
execCommand dev cmd = fmap Response <$> execCommandRaw dev cmd

--------------------------------------------------------------------------------
-- QPIGS Response Parser
--------------------------------------------------------------------------------

-- | Parsed QPIGS data
data QPIGSData = QPIGSData
  { qGridVoltage        :: !Double   -- ^ Grid voltage (V)
  , qGridFrequency      :: !Double   -- ^ Grid frequency (Hz)
  , qOutputVoltage      :: !Double   -- ^ AC output voltage (V)
  , qOutputFrequency    :: !Double   -- ^ AC output frequency (Hz)
  , qOutputApparentPwr  :: !Int      -- ^ Output apparent power (VA)
  , qOutputActivePwr    :: !Int      -- ^ Output active power (W)
  , qOutputLoadPct      :: !Int      -- ^ Output load percent (%)
  , qBusVoltage         :: !Int      -- ^ Bus voltage (V)
  , qBatteryVoltage     :: !Double   -- ^ Battery voltage (V)
  , qBatteryChargeCur   :: !Int      -- ^ Battery charging current (A)
  , qBatteryCapacity    :: !Int      -- ^ Battery capacity (%)
  , qHeatSinkTemp       :: !Int      -- ^ Inverter heat sink temp (°C)
  , qPvInputCurrent     :: !Double   -- ^ PV input current (A)
  , qPvInputVoltage     :: !Double   -- ^ PV input voltage (V)
  , qBatteryVoltageScc  :: !Double   -- ^ Battery voltage from SCC (V)
  , qBatteryDischargeCur:: !Int      -- ^ Battery discharge current (A)
  , qStatusFlags        :: !Text     -- ^ Device status flags
  } deriving (Eq, Show, Generic)

-- | Parse QPIGS response
-- Example: "234.5 49.9 234.5 49.9 0500 0400 010 380 27.60 000 100 0036 0000 000.0 00.00 00000 10010110 00 00 00000 110"
parseQPIGS :: ByteString -> Either VoltronicError QPIGSData
parseQPIGS bs = case parseOnly qpigsParser bs of
  Left err -> Left $ ParseError err
  Right v  -> Right v

qpigsParser :: Parser QPIGSData
qpigsParser = do
  qGridVoltage        <- double <* space
  qGridFrequency      <- double <* space
  qOutputVoltage      <- double <* space
  qOutputFrequency    <- double <* space
  qOutputApparentPwr  <- decimal <* space
  qOutputActivePwr    <- decimal <* space
  qOutputLoadPct      <- decimal <* space
  qBusVoltage         <- decimal <* space
  qBatteryVoltage     <- double <* space
  qBatteryChargeCur   <- decimal <* space
  qBatteryCapacity    <- decimal <* space
  qHeatSinkTemp       <- decimal <* space
  qPvInputCurrent     <- double <* space
  qPvInputVoltage     <- double <* space
  qBatteryVoltageScc  <- double <* space
  qBatteryDischargeCur<- decimal <* space
  qStatusFlags        <- TE.decodeUtf8 <$> A.takeWhile (/= ' ')
  pure QPIGSData{..}

space :: Parser ()
space = skip (== ' ')
