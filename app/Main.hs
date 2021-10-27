{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import qualified Paths_mgenv
import Grid.Viz
import Grid.Sample

import Diagrams.Prelude (unLoc, names) 
import Diagrams.Backend.SVG.CmdLine
import Control.Monad.Bayes.Sampler
import qualified Algebra.Graph.Labelled as G
import qualified Algebra.Graph.ToGraph as TG
import Text.Pretty.Simple (pPrint)
import Data.Typeable
import Diagrams.TwoD.Text (Text)


main :: IO ()
main = do
  gridSpec <- sampleIO $ sampleGridSpec
  grid <- sampleIO $ generateGrid gridSpec
  let state = (initGridState grid)
  pPrint gridSpec
  pPrint grid
