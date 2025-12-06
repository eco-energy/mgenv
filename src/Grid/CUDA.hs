{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Grid-level CUDA simulation interface
--
-- This module provides integration between the Grid generation system
-- and the CUDA parallel simulation backend.
module Grid.CUDA
  ( -- * Parallel Simulation Functions
    simulateGridCUDA
  , simulateGridCUDAFor
  , simulateGridCUDAStream
    -- * Batch Simulation
  , simulateMultipleGridsCUDA
    -- * Configuration
  , GridCUDAConfig(..)
  , defaultGridCUDAConfig
    -- * Result Types
  , GridSimResult(..)
  , GridSimTimeSeries(..)
    -- * Re-exports
  , CUDAConfig(..)
  , defaultCUDAConfig
  ) where

import Control.Monad (forM)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)

import Grid.Sample (SampledGrid(..), GridSpec'(..), generateGrid, sampleGridSpec)
import Grid.HH (HHSpec(..), NodeId)
import Physics.Storage (BatteryState(..))

import CUDA
  ( runCUDASimulation
  , runCUDASimulationN
  , runCUDASimulationStream
  , CUDAConfig(..)
  , defaultCUDAConfig
  , SimulationResult(..)
  , SimulationTimeSeries(..)
  , GPUGridSpec
  , gridToGPU
  )

import Control.Monad.Bayes.Class (MonadSample)

-- | Configuration for grid-level CUDA simulations
data GridCUDAConfig = GridCUDAConfig
  { gccCUDAConfig    :: !CUDAConfig   -- ^ Underlying CUDA config
  , gccWarmupSteps   :: !Int          -- ^ Number of warmup steps (not recorded)
  , gccRecordEvery   :: !Int          -- ^ Record results every N steps
  , gccParallelGrids :: !Int          -- ^ Number of grids to simulate in parallel
  } deriving (Eq, Show)

-- | Default grid CUDA configuration
defaultGridCUDAConfig :: GridCUDAConfig
defaultGridCUDAConfig = GridCUDAConfig
  { gccCUDAConfig = defaultCUDAConfig
  , gccWarmupSteps = 100
  , gccRecordEvery = 1
  , gccParallelGrids = 1
  }

-- | Result from a grid simulation
data GridSimResult = GridSimResult
  { gsrNodeStates   :: ![(NodeId, BatteryState, Double, Double)]  -- ^ (NodeId, Battery, Gen, Con)
  , gsrTotalReward  :: !Double
  , gsrSimTime      :: !Double
  , gsrGridSize     :: !Int
  } deriving (Show)

-- | Time series from a grid simulation
data GridSimTimeSeries = GridSimTimeSeries
  { gstResults     :: ![GridSimResult]
  , gstTotalTime   :: !Double
  , gstTotalSteps  :: !Int
  } deriving (Show)

-- | Simulate a single grid for one step using CUDA
simulateGridCUDA
  :: SampledGrid              -- ^ Grid to simulate
  -> UTCTime                  -- ^ Start time
  -> IO GridSimResult
simulateGridCUDA grid startTime = do
  result <- runCUDASimulation defaultCUDAConfig grid startTime

  -- Count nodes in grid
  let SampledGrid g = grid
      nodeCount = length $ vertexListFromGraph g

  return $ GridSimResult
    { gsrNodeStates = []  -- Would need full conversion
    , gsrTotalReward = srTotalReward result
    , gsrSimTime = srTime result
    , gsrGridSize = nodeCount
    }
  where
    vertexListFromGraph = undefined  -- Would use LG.vertexList

-- | Simulate a grid for a specified number of seconds
simulateGridCUDAFor
  :: GridCUDAConfig           -- ^ Configuration
  -> SampledGrid              -- ^ Grid to simulate
  -> UTCTime                  -- ^ Start time
  -> Double                   -- ^ Duration in seconds
  -> IO GridSimTimeSeries
simulateGridCUDAFor config grid startTime duration = do
  let
    deltaT = cudaDeltaT (gccCUDAConfig config)
    numSteps = ceiling (duration / deltaT)

  timeSeries <- runCUDASimulationN (gccCUDAConfig config) grid startTime numSteps

  -- Convert to grid-level results
  let results = map convertResult (stsResults timeSeries)

  return $ GridSimTimeSeries
    { gstResults = results
    , gstTotalTime = stsTotalTime timeSeries
    , gstTotalSteps = stsStepCount timeSeries
    }
  where
    convertResult SimulationResult{..} = GridSimResult
      { gsrNodeStates = []
      , gsrTotalReward = srTotalReward
      , gsrSimTime = srTime
      , gsrGridSize = 0  -- Would need to track
      }

-- | Simulate a grid with streaming results
simulateGridCUDAStream
  :: GridCUDAConfig                     -- ^ Configuration
  -> SampledGrid                        -- ^ Grid to simulate
  -> UTCTime                            -- ^ Start time
  -> (GridSimResult -> IO Bool)         -- ^ Callback (return False to stop)
  -> IO GridSimTimeSeries
simulateGridCUDAStream config grid startTime callback = do
  timeSeries <- runCUDASimulationStream (gccCUDAConfig config) grid startTime callback'

  let results = map convertResult (stsResults timeSeries)

  return $ GridSimTimeSeries
    { gstResults = results
    , gstTotalTime = stsTotalTime timeSeries
    , gstTotalSteps = stsStepCount timeSeries
    }
  where
    callback' simResult = do
      let gridResult = convertResult simResult
      callback gridResult

    convertResult SimulationResult{..} = GridSimResult
      { gsrNodeStates = []
      , gsrTotalReward = srTotalReward
      , gsrSimTime = srTime
      , gsrGridSize = 0
      }

-- | Simulate multiple grids in parallel
-- Each grid runs on a separate CUDA stream
simulateMultipleGridsCUDA
  :: GridCUDAConfig           -- ^ Configuration
  -> [SampledGrid]            -- ^ Grids to simulate
  -> UTCTime                  -- ^ Start time
  -> Int                      -- ^ Number of steps per grid
  -> IO [GridSimTimeSeries]
simulateMultipleGridsCUDA config grids startTime numSteps =
  mapConcurrently simulateOne grids
  where
    simulateOne grid = do
      timeSeries <- runCUDASimulationN (gccCUDAConfig config) grid startTime numSteps
      let results = map convertResult (stsResults timeSeries)
      return $ GridSimTimeSeries
        { gstResults = results
        , gstTotalTime = stsTotalTime timeSeries
        , gstTotalSteps = stsStepCount timeSeries
        }

    convertResult SimulationResult{..} = GridSimResult
      { gsrNodeStates = []
      , gsrTotalReward = srTotalReward
      , gsrSimTime = srTime
      , gsrGridSize = 0
      }
