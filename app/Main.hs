{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import qualified Paths_mgenv
import Viz
import Env
import Grid

import Diagrams.Backend.SVG.CmdLine
import Control.Monad.Bayes.Sampler


main :: IO ()
main = do
  gridSpec <- sampleIO $ sampleGridSpec
  grid <- sampleIO $ generateGrid gridSpec
  mainWith $ gridD $ mkGridViz grid (initGridState grid)
