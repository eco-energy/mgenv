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

-- CUDA imports for parallel simulation
import CUDA
  ( runCUDASimulation
  , runCUDASimulationN
  , defaultCUDAConfig
  , CUDAConfig(..)
  , SimulationResult(..)
  , SimulationTimeSeries(..)
  )
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  gridSpec <- sampleIO $ sampleGridSpec
  grid <- sampleIO $ generateGrid gridSpec

  pPrint gridSpec
  pPrint grid

  -- Run CUDA simulation if requested
  case args of
    ["--cuda"] -> runCUDAMode grid
    ["--cuda-steps", n] -> runCUDAStepsMode grid (read n)
    _ -> putStrLn "Use --cuda for single step or --cuda-steps N for N steps"

-- | Run a single CUDA simulation step
runCUDAMode :: SampledGrid -> IO ()
runCUDAMode grid = do
  putStrLn "\n=== Running Parallel CUDA Simulation ==="
  startTime <- getCurrentTime

  result <- runCUDASimulation defaultCUDAConfig grid startTime

  putStrLn $ "Simulation time: " ++ show (srTime result) ++ " seconds"
  putStrLn $ "Total reward: " ++ show (srTotalReward result)
  putStrLn "CUDA simulation completed successfully!"

-- | Run multiple CUDA simulation steps
runCUDAStepsMode :: SampledGrid -> Int -> IO ()
runCUDAStepsMode grid numSteps = do
  putStrLn $ "\n=== Running " ++ show numSteps ++ " Parallel CUDA Simulation Steps ==="
  startTime <- getCurrentTime

  timeSeries <- runCUDASimulationN defaultCUDAConfig grid startTime numSteps

  putStrLn $ "Total simulated time: " ++ show (stsTotalTime timeSeries) ++ " seconds"
  putStrLn $ "Steps executed: " ++ show (stsStepCount timeSeries)

  -- Print summary of rewards
  let rewards = map srTotalReward (stsResults timeSeries)
      avgReward = sum rewards / fromIntegral (length rewards)
  putStrLn $ "Average reward: " ++ show avgReward
  putStrLn "CUDA simulation completed successfully!"
