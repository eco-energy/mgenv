{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import qualified Paths_mgenv
import Grid.Viz
import Grid.Env
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
  --pPrint gridSpec
  --pPrint grid
  --print state
      vizGrid = mkGridViz grid state
      e (GridViz g) = (\(e, h, h') ->
                         (joinTHH)
                         (e)
                         (uncurry householdD h)
                         (uncurry householdD h))
                      . head $ G.edgeList g
      n (GridViz g) = uncurry householdD $ (\(_, h, _) -> h) . head $ G.edgeList g
      d = gridD (0, 0) vizGrid
      n' = n vizGrid
      e' = e vizGrid
  --print $ length $ G.edgeList . (\(GridState g) -> g) $ state
  --print $ length $ G.edgeList . (\(SampledGrid g) -> g) $ grid
  --print $ length $ n vizGrid
  print $ names (unLoc d)
  mainWith (unLoc $ d) -- $ 
