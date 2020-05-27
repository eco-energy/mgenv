{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import qualified Paths_mgenv
import Viz
import Env
import Grid

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Control.Monad.Bayes.Sampler
import qualified Algebra.Graph.Labelled as G
import qualified Algebra.Graph.ToGraph as TG
import Text.Pretty.Simple (pPrint)



main :: IO ()
main = do
  gridSpec <- sampleIO $ sampleGridSpec
  grid <- sampleIO $ generateGrid defGridSpec
  let state = (initGridState grid)
  pPrint gridSpec
  pPrint ((\(SampledGrid g) -> TG.edgeCount g) grid)
  --print state
  mainWith $ gridD $ mkGridViz grid state
