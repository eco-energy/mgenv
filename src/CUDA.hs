{-# LANGUAGE FlexibleContexts #-}

-- | CUDA Module for Parallel Grid Simulations
--
-- This module provides GPU-accelerated simulation capabilities for microgrid
-- environments. It enables parallel execution of household simulations across
-- all nodes in a grid simultaneously on CUDA-capable NVIDIA GPUs.
--
-- = Single Environment Usage
--
-- @
-- import CUDA
-- import Grid.Sample (generateGrid, sampleGridSpec)
-- import Prob.Randomizable (sampleIO)
-- import Data.Time.Clock (getCurrentTime)
--
-- main :: IO ()
-- main = do
--   gridSpec <- sampleIO $ sampleGridSpec
--   grid <- sampleIO $ generateGrid gridSpec
--   startTime <- getCurrentTime
--   result <- runCUDASimulation defaultCUDAConfig grid startTime
--   print $ srTotalReward result
-- @
--
-- = Vectorized Environment (PufferLib-style)
--
-- For high-throughput RL training, use the vectorized environment interface
-- which runs thousands of independent simulations in parallel:
--
-- @
-- import CUDA
--
-- main :: IO ()
-- main = do
--   grid <- sampleIO $ generateGrid =<< sampleGridSpec
--
--   -- Create 4096 parallel environments
--   env <- makeVecEnv 4096 grid defaultCUDAConfig
--
--   -- Reset and get initial observations
--   (obs, _) <- vecReset env
--
--   -- Training loop
--   forever $ do
--     let actions = ... -- Your policy network output: [4096, num_nodes]
--     StepResult obs' rewards dones _ _ <- vecStep env actions
--     -- ... update policy ...
-- @
--
-- = Architecture
--
-- The CUDA simulation system is organized into three layers:
--
-- 1. 'CUDA.Types' - GPU-compatible data types (including vectorized types)
-- 2. 'CUDA.Kernels' - Low-level GPU computation kernels (single and batched)
-- 3. 'CUDA.Simulation' - High-level simulation orchestration and VecEnv
--
-- = Requirements
--
-- * NVIDIA GPU with CUDA support
-- * CUDA toolkit installed
-- * accelerate-llvm-ptx backend
--
module CUDA
  ( -- * Single Environment Simulation
    runCUDASimulation
  , runCUDASimulationN
  , runCUDASimulationStream

    -- * Vectorized Environment (PufferLib-style)
  , VecEnv(..)
  , makeVecEnv
  , makeVecEnvFromGrids
  , vecStep
  , vecReset
  , vecClose
  , vecGetObs
  , VecEnvConfig(..)
  , VecEnvState(..)
  , StepResult(..)

    -- * Configuration
  , CUDAConfig(..)
  , defaultCUDAConfig
  , CUDABackend(..)

    -- * Results
  , SimulationResult(..)
  , SimulationTimeSeries(..)

    -- * Grid Conversion
  , gridToGPU
  , gpuToGrid
  , initGPUGridState

    -- * GPU Data Types
  , GPUGridSpec(..)
  , GPUGridState(..)
  , GPUHHSpec(..)
  , GPUHHState(..)
  , GPUBatterySpec(..)
  , GPUBatteryState(..)
  , GPUPVSpec(..)
  , GPUSimParams(..)
  , GPUTransmissionSpec(..)
  , GPULoad(..)
  , GPULoadState(..)
  , GR

    -- * Type Conversions
  , hhSpecToGPU
  , batterySpecToGPU
  , pvSpecToGPU

    -- * Re-exports
  , module CUDA.Types
  , module CUDA.Kernels
  , module CUDA.Simulation
  ) where

import CUDA.Types
import CUDA.Kernels
import CUDA.Simulation
