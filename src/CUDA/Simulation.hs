{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | High-level interface for parallel CUDA grid simulations
--
-- This module provides the main entry points for executing microgrid
-- simulations in parallel on CUDA-capable GPUs. It supports both
-- single-environment simulation and PufferLib-style vectorized environments
-- for high-throughput RL training.
--
-- = Vectorized Environment (VecEnv)
--
-- The 'VecEnv' interface enables running thousands of independent grid
-- simulations in parallel on GPU:
--
-- @
-- env <- makeVecEnv 4096 gridSpec  -- 4096 parallel environments
-- (obs, _) <- vecReset env
-- loop $ do
--   actions <- policy obs           -- Your RL policy
--   (obs', rewards, dones, _, _) <- vecStep env actions
--   ... training logic ...
-- @
--
module CUDA.Simulation
  ( -- * Simulation Execution (Single Env)
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
    -- * Grid Conversion
  , gridToGPU
  , gpuToGrid
  , hhSpecToGPU
  , batterySpecToGPU
  , pvSpecToGPU
    -- * State Initialization
  , initGPUGridState
    -- * Simulation Results
  , SimulationResult(..)
  , SimulationTimeSeries(..)
    -- * Configuration
  , CUDAConfig(..)
  , defaultCUDAConfig
    -- * Backend Selection
  , CUDABackend(..)
  , withCUDABackend
  ) where

import Prelude hiding (map, zipWith, replicate)
import qualified Prelude as P

import Data.Array.Accelerate as A hiding (Vector)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as PTX
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as AIO

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Control.Monad (foldM)
import Control.Concurrent.Async (async, wait, Async)
import Data.IORef
import Data.Time.Clock (UTCTime, diffUTCTime, addUTCTime, NominalDiffTime, utctDay, utctDayTime)
import Data.Time.Calendar (toGregorian, Day)
import Data.Time.LocalTime (todHour, todMin, todSec, timeOfDayToTime, timeToTimeOfDay)

import qualified Algebra.Graph.Labelled as LG

import CUDA.Types
import CUDA.Kernels

import Grid.Sample (SampledGrid(..))
import Grid.HH (HHSpec(..), NodeId)
import Physics.Storage (BatterySpec(..), BatteryState(..))
import Physics.PV (PVSpec(..), Mount(..), ModuleType(..))
import Physics.Consumption (ConsumptionSpec(..), Load(..))
import Physics.Transmission (TransmissionSpec(..))
import qualified Data.Astro.Types as A (GeographicCoordinates(..), DecimalDegrees(..))

-- | CUDA execution backend selection
data CUDABackend
  = PTXBackend      -- ^ NVIDIA PTX backend (requires CUDA)
  | NativeBackend   -- ^ CPU fallback (for testing)
  deriving (Eq, Show)

-- | CUDA simulation configuration
data CUDAConfig = CUDAConfig
  { cudaBackend      :: !CUDABackend  -- ^ Which backend to use
  , cudaBlockSize    :: !Int          -- ^ CUDA block size (default 256)
  , cudaStreamCount  :: !Int          -- ^ Number of concurrent streams
  , cudaDeltaT       :: !Double       -- ^ Time step in seconds
  , cudaMaxSteps     :: !Int          -- ^ Maximum simulation steps
  } deriving (Eq, Show)

-- | Default CUDA configuration
defaultCUDAConfig :: CUDAConfig
defaultCUDAConfig = CUDAConfig
  { cudaBackend = PTXBackend
  , cudaBlockSize = 256
  , cudaStreamCount = 4
  , cudaDeltaT = 1.0
  , cudaMaxSteps = 86400  -- One day of seconds
  }

-- | Result of a single simulation step
data SimulationResult = SimulationResult
  { srStates      :: !(A.Vector GPUHHState)  -- ^ Final states of all households
  , srTime        :: !Double                  -- ^ Simulation time
  , srTotalReward :: !Double                  -- ^ Aggregate reward
  } deriving (Show)

-- | Time series of simulation results
data SimulationTimeSeries = SimulationTimeSeries
  { stsResults    :: ![SimulationResult]     -- ^ List of results per time step
  , stsTotalTime  :: !Double                 -- ^ Total simulated time
  , stsStepCount  :: !Int                    -- ^ Number of steps executed
  } deriving (Show)

-- | Convert Mount type to GPU int
mountToInt :: Mount -> Int
mountToInt OpenRack = 0
mountToInt CloseRoofMount = 1
mountToInt InsulatedBack = 2
mountToInt Tracker = 3

-- | Convert ModuleType to GPU int
moduleTypeToInt :: ModuleType -> Int
moduleTypeToInt GlassCellGlass = 0
moduleTypeToInt GlassCellPolymerSheet = 1
moduleTypeToInt PolymerThinFilmSteel = 2
moduleTypeToInt LinearConcentrator = 3

-- | Convert CPU BatterySpec to GPU format
batterySpecToGPU :: BatterySpec -> GPUBatterySpec
batterySpecToGPU BatterySpec{..} = GPUBatterySpec
  { gbsChargeEff = fst coloumbicEff
  , gbsDischargeEff = snd coloumbicEff
  , gbsTotalCapacity = totalChargeCapacity
  , gbsQMin = qMin
  , gbsQMax = qMax
  , gbsVNominal = vNominal
  , gbsVMin = vMin
  , gbsVMax = vMax
  , gbsDelVDisAtI = delVDisAtI
  , gbsIDis = iDis
  , gbsDelVChgAtI = delVChgAtI
  , gbsIChg = iChg
  }

-- | Convert CPU PVSpec to GPU format
pvSpecToGPU :: PVSpec -> GPUPVSpec
pvSpecToGPU PVSpec{..} = GPUPVSpec
  { gpvsArrAzimuth = arrAzimuth
  , gpvsArrTilt = arrTilt
  , gpvsTempCorrection = tempCorrection
  , gpvsPower = power
  , gpvsMountType = mountToInt mount
  , gpvsModuleType = moduleTypeToInt moduleType
  }

-- | Convert CPU HHSpec to GPU format
hhSpecToGPU :: HHSpec -> GPUHHSpec
hhSpecToGPU HHSpec{..} = GPUHHSpec
  { ghsNodeId = nId
  , ghsLatitude = lat'
  , ghsLongitude = lon'
  , ghsGridX = fst gridLoc
  , ghsGridY = snd gridLoc
  , ghsBattery = batterySpecToGPU storage
  , ghsPV = pvSpecToGPU generation
  }
  where
    A.GeoC (A.DD lat') (A.DD lon') = loc

-- | Convert CPU TransmissionSpec to GPU format
transmissionSpecToGPU :: TransmissionSpec -> GPUTransmissionSpec
transmissionSpecToGPU TransmissionSpec{..} = GPUTransmissionSpec
  { gtsWireLength = wireLength
  , gtsCrossSection = crossSection
  , gtsResistivity = resistivity
  }

-- | Convert a SampledGrid to GPU format
gridToGPU :: SampledGrid -> IO GPUGridSpec
gridToGPU (SampledGrid graph) = do
  let
    -- Extract vertices (households) from graph
    vertices = LG.vertexList graph

    -- Extract edges from graph
    edgesList = LG.edgeList graph

    -- Convert to GPU types
    gpuHouseholds = P.map hhSpecToGPU vertices

    -- Create edge pairs and transmission specs
    gpuEdges = P.map (\(tx, h1, h2) -> (nId h1, nId h2)) edgesList
    gpuTransmission = P.map (\(tx, _, _) -> transmissionSpecToGPU tx) edgesList

  -- Convert to Accelerate arrays
  let
    hhArray = A.fromList (Z :. length gpuHouseholds) gpuHouseholds
    edgeArray = A.fromList (Z :. length gpuEdges) gpuEdges
    txArray = A.fromList (Z :. length gpuTransmission) gpuTransmission

  return $ GPUGridSpec
    { ggsHouseholds = hhArray
    , ggsEdges = edgeArray
    , ggsTransmission = txArray
    }

-- | Convert GPU grid state back to CPU format
gpuToGrid :: GPUGridState -> [(NodeId, BatteryState, Double, Double)]
gpuToGrid GPUGridState{..} =
  let
    statesList = A.toList ggsStates
  in
    P.zipWith convertState [0..] statesList
  where
    convertState idx (GPUHHState bat gen con _ _) =
      let
        GPUBatteryState v soc e cp dp = bat
        cpuBatt = BatteryState v soc e cp dp
      in
        (idx, cpuBatt, gen, con)

-- | Initialize GPU grid state from specifications
initGPUGridState :: GPUGridSpec -> GPUGridState
initGPUGridState GPUGridSpec{..} =
  let
    initStates = PTX.run $ initHHStatesKernel (A.use ggsHouseholds)
    initTime = A.fromList Z [0.0 :: Double]
  in
    GPUGridState
      { ggsStates = initStates
      , ggsTime = initTime
      }

-- | Create simulation parameters from time
createSimParams :: CUDAConfig -> UTCTime -> GPUSimParams
createSimParams CUDAConfig{..} time =
  let
    dayOfYear = getDayOfYear time
    hourOfDay = getHourOfDay time
  in
    GPUSimParams
      { gspDeltaT = cudaDeltaT
      , gspAmbientTemp = 25.0   -- Default ambient temp
      , gspWindSpeed = 2.0      -- Default wind speed
      , gspDayOfYear = dayOfYear
      , gspHourOfDay = hourOfDay
      }

-- | Get day of year from UTCTime
getDayOfYear :: UTCTime -> Int
getDayOfYear time =
  let (year, month, day) = toGregorian (utctDay time)
      -- Simple approximation
  in day + (month - 1) * 30

-- | Get hour of day (with fraction) from UTCTime
getHourOfDay :: UTCTime -> Double
getHourOfDay time =
  let tod = timeToTimeOfDay (utctDayTime time)
      h = fromIntegral (todHour tod)
      m = fromIntegral (todMin tod)
      s = realToFrac (todSec tod)
  in h + m/60 + s/3600

-- | Run a single simulation step on GPU
runSimulationStep
  :: GPUGridSpec
  -> GPUSimParams
  -> GPUGridState
  -> IO GPUGridState
runSimulationStep GPUGridSpec{..} simParams GPUGridState{..} = do
  let
    -- Create simulation parameters as a scalar
    paramsScalar = A.fromList Z [simParams]

    -- Run the grid step kernel on PTX backend
    newStates = PTX.run $ gridStepKernel
      (A.use ggsHouseholds)
      (A.use paramsScalar)
      (A.use ggsStates)

    -- Update time
    currentTime = head (A.toList ggsTime)
    newTime = A.fromList Z [currentTime + gspDeltaT simParams]

  return $ GPUGridState
    { ggsStates = newStates
    , ggsTime = newTime
    }

-- | Compute aggregate reward for a state
computeReward :: GPUGridState -> Double
computeReward GPUGridState{..} =
  let
    rewardScalar = PTX.run $ aggregateRewardsKernel (A.use ggsStates)
  in
    head (A.toList rewardScalar)

-- | Execute a complete CUDA simulation for any grid
-- This is the main entry point for single-run simulation
runCUDASimulation
  :: CUDAConfig           -- ^ Simulation configuration
  -> SampledGrid          -- ^ Grid generated by the library
  -> UTCTime              -- ^ Start time
  -> IO SimulationResult  -- ^ Final simulation result
runCUDASimulation config grid startTime = do
  -- Convert grid to GPU format
  gpuGrid <- gridToGPU grid

  -- Initialize state
  let initState = initGPUGridState gpuGrid

  -- Create initial simulation parameters
  let simParams = createSimParams config startTime

  -- Run simulation step
  finalState <- runSimulationStep gpuGrid simParams initState

  -- Compute final reward
  let reward = computeReward finalState
      finalTime = head (A.toList (ggsTime finalState))

  return $ SimulationResult
    { srStates = ggsStates finalState
    , srTime = finalTime
    , srTotalReward = reward
    }

-- | Execute N simulation steps on GPU
runCUDASimulationN
  :: CUDAConfig               -- ^ Simulation configuration
  -> SampledGrid              -- ^ Grid generated by the library
  -> UTCTime                  -- ^ Start time
  -> Int                      -- ^ Number of steps
  -> IO SimulationTimeSeries  -- ^ Time series of results
runCUDASimulationN config grid startTime numSteps = do
  -- Convert grid to GPU format
  gpuGrid <- gridToGPU grid

  -- Initialize state
  let initState = initGPUGridState gpuGrid

  -- Run simulation loop
  (results, finalState) <- foldM (stepAndCollect gpuGrid startTime) ([], initState) [0..numSteps-1]

  let totalTime = P.fromIntegral numSteps * cudaDeltaT config

  return $ SimulationTimeSeries
    { stsResults = reverse results
    , stsTotalTime = totalTime
    , stsStepCount = numSteps
    }
  where
    stepAndCollect gpuGrid start (!results, !state) stepNum = do
      let
        currentTime = addUTCTime (P.fromIntegral stepNum * realToFrac (cudaDeltaT config)) start
        simParams = createSimParams config currentTime

      newState <- runSimulationStep gpuGrid simParams state

      let
        reward = computeReward newState
        simTime = head (A.toList (ggsTime newState))
        result = SimulationResult
          { srStates = ggsStates newState
          , srTime = simTime
          , srTotalReward = reward
          }

      return (result : results, newState)

-- | Execute streaming simulation with callback
-- Useful for real-time monitoring or progressive results
runCUDASimulationStream
  :: CUDAConfig                          -- ^ Simulation configuration
  -> SampledGrid                         -- ^ Grid generated by the library
  -> UTCTime                             -- ^ Start time
  -> (SimulationResult -> IO Bool)       -- ^ Callback (return False to stop)
  -> IO SimulationTimeSeries             -- ^ Final time series
runCUDASimulationStream config grid startTime callback = do
  -- Convert grid to GPU format
  gpuGrid <- gridToGPU grid

  -- Initialize state
  let initState = initGPUGridState gpuGrid

  -- Run streaming loop
  resultsRef <- newIORef ([] :: [SimulationResult])
  stepCountRef <- newIORef (0 :: Int)

  let
    loop !state !stepNum = do
      let
        currentTime = addUTCTime (P.fromIntegral stepNum * realToFrac (cudaDeltaT config)) startTime
        simParams = createSimParams config currentTime

      newState <- runSimulationStep gpuGrid simParams state

      let
        reward = computeReward newState
        simTime = head (A.toList (ggsTime newState))
        result = SimulationResult
          { srStates = ggsStates newState
          , srTime = simTime
          , srTotalReward = reward
          }

      -- Store result
      modifyIORef' resultsRef (result :)
      modifyIORef' stepCountRef (+1)

      -- Call callback
      continue <- callback result

      if continue && stepNum < cudaMaxSteps config
        then loop newState (stepNum + 1)
        else return ()

  -- Start the loop
  loop initState 0

  -- Collect results
  results <- readIORef resultsRef
  stepCount <- readIORef stepCountRef

  let totalTime = P.fromIntegral stepCount * cudaDeltaT config

  return $ SimulationTimeSeries
    { stsResults = reverse results
    , stsTotalTime = totalTime
    , stsStepCount = stepCount
    }

-- | Execute with specific CUDA backend
withCUDABackend :: CUDABackend -> IO a -> IO a
withCUDABackend PTXBackend action = action
withCUDABackend NativeBackend action = action  -- Fallback is the same for now

--------------------------------------------------------------------------------
-- Vectorized Environment (PufferLib-style)
--------------------------------------------------------------------------------

-- | Vectorized environment for parallel RL training
-- Runs num_envs independent grid simulations on GPU simultaneously
data VecEnv = VecEnv
  { veConfig      :: !VecEnvConfig           -- ^ Environment configuration
  , veState       :: !(IORef VecEnvState)    -- ^ Mutable state reference
  , veSpecs       :: !(Array DIM2 GPUHHSpec) -- ^ Grid specs [num_envs, num_nodes]
  , veCUDAConfig  :: !CUDAConfig             -- ^ CUDA configuration
  }

-- | Create a vectorized environment with N copies of the same grid
makeVecEnv
  :: Int                    -- ^ Number of parallel environments
  -> SampledGrid            -- ^ Grid template (replicated across all envs)
  -> CUDAConfig             -- ^ CUDA configuration
  -> IO VecEnv
makeVecEnv numEnvs grid cudaConfig = do
  -- Convert grid to GPU format
  gpuGrid <- gridToGPU grid

  let
    -- Get specs and replicate across environments
    singleEnvSpecs = ggsHouseholds gpuGrid
    numNodes = P.length (A.toList singleEnvSpecs)

    -- Replicate specs for all environments: [num_envs, num_nodes]
    specsList = P.concat $ P.replicate numEnvs (A.toList singleEnvSpecs)
    specs2D = A.fromList (Z :. numEnvs :. numNodes) specsList

    -- Configuration
    config = VecEnvConfig
      { vecNumEnvs = numEnvs
      , vecNumNodes = numNodes
      , vecObsDim = 6  -- soc, energy, gen, con, txIn, txOut per node
      , vecActionDim = 1  -- battery discharge rate per node
      , vecMaxEpisodeLen = cudaMaxSteps cudaConfig
      , vecAutoReset = True
      , vecSharedTopology = True
      }

  -- Initialize state
  initState <- initVecEnvState config specs2D cudaConfig
  stateRef <- newIORef initState

  return $ VecEnv
    { veConfig = config
    , veState = stateRef
    , veSpecs = specs2D
    , veCUDAConfig = cudaConfig
    }

-- | Create vectorized environment from multiple different grids
makeVecEnvFromGrids
  :: [SampledGrid]          -- ^ List of grids (one per environment)
  -> CUDAConfig             -- ^ CUDA configuration
  -> IO VecEnv
makeVecEnvFromGrids grids cudaConfig = do
  -- Convert all grids to GPU format
  gpuGrids <- P.mapM gridToGPU grids

  let
    numEnvs = P.length grids
    -- Assume all grids have same number of nodes (pad if needed in production)
    numNodes = P.length $ A.toList $ ggsHouseholds $ P.head gpuGrids

    -- Concatenate all specs: [num_envs, num_nodes]
    allSpecs = P.concatMap (A.toList . ggsHouseholds) gpuGrids
    specs2D = A.fromList (Z :. numEnvs :. numNodes) allSpecs

    config = VecEnvConfig
      { vecNumEnvs = numEnvs
      , vecNumNodes = numNodes
      , vecObsDim = 6
      , vecActionDim = 1
      , vecMaxEpisodeLen = cudaMaxSteps cudaConfig
      , vecAutoReset = True
      , vecSharedTopology = False
      }

  initState <- initVecEnvState config specs2D cudaConfig
  stateRef <- newIORef initState

  return $ VecEnv
    { veConfig = config
    , veState = stateRef
    , veSpecs = specs2D
    , veCUDAConfig = cudaConfig
    }

-- | Initialize vectorized environment state
initVecEnvState
  :: VecEnvConfig
  -> Array DIM2 GPUHHSpec
  -> CUDAConfig
  -> IO VecEnvState
initVecEnvState VecEnvConfig{..} specs cudaConfig = do
  let
    -- Initialize states on GPU
    initStates = PTX.run $ vecInitStatesKernel (A.use specs)

    -- Initialize time and step counters
    times = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs 0.0)
    stepCounts = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs (0 :: Int))
    dones = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs (0 :: Int))
    truncated = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs (0 :: Int))
    cumRewards = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs 0.0)

    -- Initialize sim params (will be updated each step)
    defaultParams = GPUSimParams
      { gspDeltaT = cudaDeltaT cudaConfig
      , gspAmbientTemp = 25.0
      , gspWindSpeed = 2.0
      , gspDayOfYear = 1
      , gspHourOfDay = 12.0
      }
    simParams = A.fromList (Z :. vecNumEnvs) (P.replicate vecNumEnvs defaultParams)

  return $ VecEnvState
    { vesStates = initStates
    , vesSpecs = specs
    , vesTimes = times
    , vesStepCounts = stepCounts
    , vesDones = dones
    , vesTruncated = truncated
    , vesCumulativeReward = cumRewards
    , vesSimParams = simParams
    }

-- | Step all environments with given actions
-- This is the main RL interface - single batched call for all envs
vecStep
  :: VecEnv
  -> Array DIM2 GR                    -- ^ Actions: [num_envs, num_nodes]
  -> IO StepResult
vecStep VecEnv{..} actions = do
  currentState <- readIORef veState
  let
    VecEnvConfig{..} = veConfig

    -- Clip actions
    clippedActions = PTX.run $ vecApplyActionsKernel (A.use actions)

    -- Step all environments
    newStates = PTX.run $ vecGridStepKernel
      (A.use veSpecs)
      (A.use $ vesSimParams currentState)
      (A.use $ vesStates currentState)
      (A.use clippedActions)

    -- Compute rewards
    (envRewards, nodeRewards) = PTX.runN $ vecComputeRewardsKernel (A.use newStates)

    -- Increment step counts
    newStepCounts = PTX.run $ A.map (+1) (A.use $ vesStepCounts currentState)

    -- Check done/truncated
    (newDones, newTruncated) = PTX.runN $
      vecCheckDoneKernel (A.use newStates) (A.use newStepCounts) (A.lift vecMaxEpisodeLen)

    -- Auto-reset done environments
    finalStates = if vecAutoReset
      then PTX.run $ vecResetKernel (A.use veSpecs) (A.use newDones) (A.use newStates)
      else newStates

    -- Reset step counts for done envs
    finalStepCounts = if vecAutoReset
      then PTX.run $ A.zipWith (\d s -> cond (d A.== 1) 0 s)
             (A.use newDones) (A.use newStepCounts)
      else newStepCounts

    -- Extract observations
    obs = PTX.run $ vecExtractObsKernel (A.use finalStates) (A.lift vecObsDim)

    -- Update times
    newTimes = PTX.run $ A.zipWith (+) (A.use $ vesTimes currentState)
                 (A.map gspDeltaT (A.use $ vesSimParams currentState))

  -- Update state
  let
    updatedState = currentState
      { vesStates = finalStates
      , vesTimes = newTimes
      , vesStepCounts = finalStepCounts
      , vesDones = newDones
      , vesTruncated = newTruncated
      }

  writeIORef veState updatedState

  return $ StepResult
    { srObs = obs
    , srRewards = envRewards
    , srDones = newDones
    , srTruncated = newTruncated
    , srNodeRewards = nodeRewards
    }

-- | Reset all environments and return initial observations
vecReset
  :: VecEnv
  -> IO (Array DIM2 GR, VecEnvState)  -- ^ (observations, new state)
vecReset VecEnv{..} = do
  let VecEnvConfig{..} = veConfig

  -- Re-initialize state
  newState <- initVecEnvState veConfig veSpecs veCUDAConfig

  -- Extract observations
  let obs = PTX.run $ vecExtractObsKernel (A.use $ vesStates newState) (A.lift vecObsDim)

  writeIORef veState newState

  return (obs, newState)

-- | Get current observations without stepping
vecGetObs :: VecEnv -> IO (Array DIM2 GR)
vecGetObs VecEnv{..} = do
  currentState <- readIORef veState
  let VecEnvConfig{..} = veConfig
  return $ PTX.run $ vecExtractObsKernel (A.use $ vesStates currentState) (A.lift vecObsDim)

-- | Close the environment (cleanup)
vecClose :: VecEnv -> IO ()
vecClose _ = return ()  -- No explicit cleanup needed for Accelerate

-- | Helper to run two Acc computations and return both results
runN :: (Arrays a, Arrays b) => (Acc a, Acc b) -> (a, b)
runN (a, b) = (PTX.run a, PTX.run b)
