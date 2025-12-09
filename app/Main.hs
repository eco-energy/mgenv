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
import Control.Monad (forM_, replicateM_)

-- CUDA imports for parallel simulation
import CUDA
  ( -- Single env
    runCUDASimulation
  , runCUDASimulationN
  , defaultCUDAConfig
  , CUDAConfig(..)
  , SimulationResult(..)
  , SimulationTimeSeries(..)
    -- Vectorized env (PufferLib-style)
  , VecEnv(..)
  , VecEnvConfig(..)
  , StepResult(..)
  , makeVecEnv
  , vecStep
  , vecReset
  , vecClose
  , GR
  )
import qualified Data.Array.Accelerate as A
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
    ["--vecenv", n] -> runVecEnvMode grid (read n)
    ["--vecenv-bench", numEnvs, numSteps] ->
      runVecEnvBenchmark grid (read numEnvs) (read numSteps)
    _ -> do
      putStrLn "Usage:"
      putStrLn "  --cuda                     Single CUDA simulation step"
      putStrLn "  --cuda-steps N             N sequential CUDA steps"
      putStrLn "  --vecenv N                 Vectorized env with N parallel envs"
      putStrLn "  --vecenv-bench N_ENVS N_STEPS  Benchmark vectorized env"

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

-- | Run vectorized environment demo (PufferLib-style)
runVecEnvMode :: SampledGrid -> Int -> IO ()
runVecEnvMode grid numEnvs = do
  putStrLn $ "\n=== Vectorized Environment: " ++ show numEnvs ++ " parallel envs ==="

  -- Create vectorized environment
  env <- makeVecEnv numEnvs grid defaultCUDAConfig

  let config = veConfig env
  putStrLn $ "Num environments: " ++ show (vecNumEnvs config)
  putStrLn $ "Nodes per env: " ++ show (vecNumNodes config)
  putStrLn $ "Obs dim per node: " ++ show (vecObsDim config)
  putStrLn $ "Total parallel computations per step: " ++
             show (vecNumEnvs config * vecNumNodes config)

  -- Reset and get initial observations
  (initObs, _) <- vecReset env
  let A.Z A.:. nEnvs A.:. obsDim = A.arrayShape initObs
  putStrLn $ "\nInitial obs shape: [" ++ show nEnvs ++ ", " ++ show obsDim ++ "]"

  -- Run a few steps with random actions
  putStrLn "\nRunning 10 steps with zero actions..."
  let numNodes = vecNumNodes config
      zeroActions = A.fromList (A.Z A.:. numEnvs A.:. numNodes) $
                    replicate (numEnvs * numNodes) (0.0 :: GR)

  forM_ [1..10] $ \step -> do
    result <- vecStep env zeroActions
    let totalReward = sum $ A.toList (srRewards result)
        numDone = sum $ A.toList (srDones result)
    putStrLn $ "Step " ++ show step ++
               ": total_reward=" ++ show totalReward ++
               ", envs_done=" ++ show numDone

  vecClose env
  putStrLn "\nVectorized environment demo completed!"

-- | Benchmark vectorized environment throughput
runVecEnvBenchmark :: SampledGrid -> Int -> Int -> IO ()
runVecEnvBenchmark grid numEnvs numSteps = do
  putStrLn $ "\n=== VecEnv Benchmark: " ++ show numEnvs ++ " envs × " ++
             show numSteps ++ " steps ==="

  -- Create environment
  env <- makeVecEnv numEnvs grid defaultCUDAConfig
  let config = veConfig env
      numNodes = vecNumNodes config
      totalOps = numEnvs * numNodes * numSteps

  putStrLn $ "Total operations: " ++ show totalOps

  -- Prepare actions (zeros for simplicity)
  let zeroActions = A.fromList (A.Z A.:. numEnvs A.:. numNodes) $
                    replicate (numEnvs * numNodes) (0.0 :: GR)

  -- Reset
  _ <- vecReset env

  -- Time the benchmark
  startTime <- getCurrentTime

  -- Run steps
  replicateM_ numSteps $ do
    _ <- vecStep env zeroActions
    return ()

  endTime <- getCurrentTime

  let elapsed = realToFrac (endTime `diffUTCTime` startTime) :: Double
      stepsPerSec = fromIntegral numSteps / elapsed
      envsStepsPerSec = fromIntegral (numEnvs * numSteps) / elapsed

  putStrLn $ "\nResults:"
  putStrLn $ "  Elapsed time: " ++ show elapsed ++ " seconds"
  putStrLn $ "  Steps/second: " ++ show stepsPerSec
  putStrLn $ "  Env-steps/second: " ++ show envsStepsPerSec
  putStrLn $ "  Node-ops/second: " ++ show (envsStepsPerSec * fromIntegral numNodes)

  vecClose env
  where
    diffUTCTime = Data.Time.Clock.diffUTCTime
