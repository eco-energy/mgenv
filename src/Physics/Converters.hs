{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Converters where

import Physics.Units
import GHC.Generics (Generic)

import Streamly
import Streamly.Prelude as S

import Control.Monad.IO.Class (MonadIO(liftIO))

type DutyCycle = R

data PIConstants = PIConstants { kPI :: R, kII :: R } deriving (Eq, Ord, Show, Generic)

data OperationMode = CCC | DAVDC | CIVD deriving (Eq, Ord, Show, Generic)

dummyCurrent :: MonadAsync m => SerialT m Amp
dummyCurrent = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ (10 :: Amp)

dummyGridV :: MonadAsync m => SerialT m V
dummyGridV = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ (60 :: V)

dummyBatteryV :: MonadAsync m => SerialT m V
dummyBatteryV = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ (12 :: V)


{--
dutyCycle :: PIConstants -> Serial Amp -> Serial Amp -> Serial DutyCycle
dutyCycle PIConstants {..} iRef iIn = do
  iRefT <- S.take 1 iRef
  iInT <- S.take 1 iIn
  let
    piPart = (kPI*(iRefT - iInT))
  return $ liftIO (S.sum $ S.zipWith (-) iIn iRef) >>= (\integrated -> piPart + (kII* integrated))

--}
--iRef :: OperationMode -> 
