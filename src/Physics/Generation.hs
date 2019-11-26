{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Physics.Generation  where

import Physics.PV (samplePVSpec, PVSpec, runPV)
import Control.Monad.Bayes.Class
import GHC.Generics (Generic)


data GenSpec = GenSpec PVSpec deriving (Eq, Ord, Show, Generic)

sampleGenSpec :: (MonadSample m) => m GenSpec
sampleGenSpec = do
  pv <- samplePVSpec
  return $ GenSpec pv

runGen loc (GenSpec pv) = runPV loc pv
