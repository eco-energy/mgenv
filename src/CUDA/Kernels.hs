{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RebindableSyntax #-}

-- | GPU computation kernels for parallel microgrid simulation
--
-- This module contains the core CUDA kernels that execute on the GPU.
-- It provides both single-environment kernels and vectorized (batched)
-- kernels for PufferLib-style parallel RL training.
--
-- = Vectorized Kernels
--
-- The @vec*@ kernels operate on 2D arrays with shape @[num_envs, num_nodes]@,
-- enabling massive parallelism across both environment instances and grid nodes.
--
module CUDA.Kernels
  ( -- * Battery Kernels
    batteryStateKernel
  , batteryStepKernel
    -- * PV Generation Kernels
  , pvGenerationKernel
  , effectiveIrradianceKernel
  , cellTempKernel
  , maxPowerPointKernel
    -- * Consumption Kernels
  , consumptionKernel
  , updateLoadStatesKernel
    -- * Household Kernels
  , householdStepKernel
    -- * Grid-level Kernels (Single Env)
  , gridStepKernel
  , aggregateRewardsKernel
    -- * Vectorized Kernels (PufferLib-style)
  , vecGridStepKernel
  , vecInitStatesKernel
  , vecResetKernel
  , vecComputeRewardsKernel
  , vecExtractObsKernel
  , vecCheckDoneKernel
  , vecApplyActionsKernel
    -- * Graph Message Passing Kernels
  , gatherNodeFeatures
  , scatterAddMessages
  , computePowerFlowWithLosses
  , messagePassingRound
  , vecMessagePassingKernel
  , vecApplyPowerExchange
    -- * Utility Kernels
  , initBatteryStatesKernel
  , initHHStatesKernel
  ) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits as A
import Prelude (($))
import CUDA.Types

-- | Pi constant for GPU computations
pi_ :: Exp GR
pi_ = 3.14159265358979323846

-- | Convert degrees to radians on GPU
toRadians :: Exp GR -> Exp GR
toRadians d = (A.mod' d 360) * pi_ / 180

-- | Convert radians to degrees on GPU
toDegrees :: Exp GR -> Exp GR
toDegrees r = A.mod' ((r * 180 / pi_) + 360) 360.0

-- | Compute stored energy from battery parameters
storedEnergyKernel :: Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR
storedEnergyKernel qmax vNom soc qmin = (qmax * vNom) * (soc - (qmin / qmax))

-- | Internal resistance calculation
internalResistanceKernel :: Exp GR -> Exp GR -> Exp GR
internalResistanceKernel deltaV refCurr = deltaV / refCurr

-- | Charge current limit calculation
chargeCurrentLimitKernel :: Exp GR -> Exp GR -> Exp GR -> Exp GR
chargeCurrentLimitKernel ocV vMax chargeR = (ocV - vMax) / chargeR

-- | Discharge current limit calculation
dischargeCurrentLimitKernel :: Exp GR -> Exp GR -> Exp GR -> Exp GR
dischargeCurrentLimitKernel ocV vMin dischargeR = (ocV - vMin) / dischargeR

-- | Power calculation (V * I)
powerKernel :: Exp GR -> Exp GR -> Exp GR
powerKernel v i = v * i

-- | Amp-hours to Coulombs conversion
ahToColoumb :: Exp GR -> Exp GR
ahToColoumb ah = ah * 3600

-- | Single battery state step kernel
-- Computes the next battery state given spec, time delta, and current
batteryStepKernel
  :: Exp GPUBatterySpec
  -> Exp GR                    -- ^ Time delta (seconds)
  -> Exp GR                    -- ^ Current (Amps)
  -> Exp GPUBatteryState
  -> Exp GPUBatteryState
batteryStepKernel
  (GPUBatterySpec_ chargeEff dischargeEff totalCap qMin qMax vNom vMin vMax delVDis iDis delVChg iChg)
  delT
  current
  (GPUBatteryState_ vt zt et _ _) =
    let
      -- Coulombic efficiency based on current direction
      ce = cond (current A.<= 0) chargeEff dischargeEff

      -- State of charge update
      ztNext = zt - (delT / ahToColoumb totalCap) - (ce * current)

      -- Voltage update (proportional to SoC change, matching CPU physics)
      -- v(t+1) = v(t) * (SoC(t+1) / SoC(t))
      -- Guard against division by zero when SoC is 0
      vtNext = cond (zt A./= 0) (vt * (ztNext / zt)) vNom

      -- Energy stored
      etNext = storedEnergyKernel totalCap vNom ztNext qMin

      -- Internal resistances
      rDis = internalResistanceKernel delVDis iDis
      rChg = internalResistanceKernel delVChg iChg

      -- Power limits
      cpNext = powerKernel vMax (chargeCurrentLimitKernel vNom vMax rChg)
      dpNext = powerKernel vMin (dischargeCurrentLimitKernel vNom vMin rDis)
    in
      GPUBatteryState_ vtNext ztNext etNext cpNext dpNext

-- | Vectorized battery state evolution kernel
-- Processes all households' batteries in parallel
batteryStateKernel
  :: Acc (Vector GPUBatterySpec)   -- ^ Battery specifications
  -> Acc (Scalar GR)               -- ^ Time delta
  -> Acc (Vector GR)               -- ^ Currents for each battery
  -> Acc (Vector GPUBatteryState)  -- ^ Current states
  -> Acc (Vector GPUBatteryState)  -- ^ Next states
batteryStateKernel specs deltaT currents states =
  A.zipWith3 (batteryStepKernel' dt) specs currents states
  where
    dt = the deltaT
    batteryStepKernel' d spec cur st = batteryStepKernel spec d cur st

-- | Module temperature calculation kernel (Sandia model)
moduleTemp :: Exp Int -> Exp Int -> Exp GR -> Exp GR -> Exp GR -> Exp GR
moduleTemp mountType modType irradiance ambientTemp ws =
  irradiance * A.exp (a + b * ws) + ambientTemp
  where
    -- Lookup coefficients based on mount and module type
    -- Simplified: using default GlassCellGlass/OpenRack values
    a = -3.47
    b = -0.0594

-- | Cell temperature calculation kernel
cellTempKernel :: Exp GR -> Exp GR -> Exp GR
cellTempKernel tMod ePOA = tMod + (ePOA / eRef) * delT
  where
    eRef = 1000 :: Exp GR
    delT = 10   :: Exp GR

-- | Effective irradiance calculation kernel
effectiveIrradianceKernel
  :: Exp GR      -- ^ Latitude
  -> Exp GR      -- ^ Longitude
  -> Exp GR      -- ^ Array azimuth
  -> Exp GR      -- ^ Array tilt
  -> Exp Int     -- ^ Day of year
  -> Exp GR      -- ^ Hour of day
  -> Exp GR      -- ^ Effective irradiance (W/m^2)
effectiveIrradianceKernel lat lon arrAzimuth arrTilt dayOfYear hourOfDay =
  let
    -- Solar position calculations (simplified)
    dayAngle = 2 * pi_ * (A.fromIntegral dayOfYear - 1) / 365

    -- Solar declination (radians)
    declination = 0.006918 - 0.399912 * A.cos dayAngle
                  + 0.070257 * A.sin dayAngle
                  - 0.006758 * A.cos (2 * dayAngle)
                  + 0.000907 * A.sin (2 * dayAngle)

    -- Hour angle
    solarNoon = 12.0
    hourAngle = toRadians $ (hourOfDay - solarNoon) * 15

    -- Latitude in radians
    latRad = toRadians lat

    -- Solar altitude angle
    sinAlt = A.sin latRad * A.sin declination
           + A.cos latRad * A.cos declination * A.cos hourAngle
    solarAltitude = A.asin (A.max (-1) (A.min 1 sinAlt))
    solarAltDeg = toDegrees solarAltitude

    -- Solar azimuth
    cosAz = (A.sin declination - A.sin solarAltitude * A.sin latRad)
          / (A.cos solarAltitude * A.cos latRad)
    solarAzimuth = A.acos (A.max (-1) (A.min 1 cosAz))
    solarAzDeg = cond (hourAngle A.< 0) (180 - toDegrees solarAzimuth) (180 + toDegrees solarAzimuth)

    -- Direct normal irradiance (simplified model)
    flux = 1160 + 75 * A.sin (2 * pi_ / 365 * (A.fromIntegral dayOfYear - 275))
    opticalDepth = 0.174 + 0.035 * A.sin (2 * pi_ / 365 * (A.fromIntegral dayOfYear - 100))
    airMassRatio = cond (solarAltDeg A.> 0) (1 / A.sin (toRadians solarAltDeg)) 999
    dni = flux * A.exp (-1 * opticalDepth * airMassRatio)

    -- Angle of incidence
    solarZenith = 90 - solarAltDeg
    aoi = A.acos $ A.cos (toRadians solarZenith) * A.cos (toRadians arrTilt)
        + A.sin (toRadians solarZenith) * A.sin (toRadians arrTilt)
        * A.cos (toRadians (solarAzDeg - arrAzimuth))

    -- Effective irradiance
    effIrr = dni * A.cos aoi
  in
    cond (solarAltDeg A.> 0) (A.max 0 effIrr) 0

-- | Maximum power point calculation kernel
maxPowerPointKernel :: Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR
maxPowerPointKernel modPower effIrr cellTemp tempCorr =
  let
    refIrr = 1000 :: Exp GR
    refTemp = 25 :: Exp GR
    lowIrrFactor = cond (effIrr A.> 125)
      (effIrr / refIrr)
      ((0.008 * effIrr * effIrr) / refIrr)
    tempFactor = 1 + tempCorr * (cellTemp - refTemp)
  in
    lowIrrFactor * modPower * tempFactor

-- | PV generation kernel for a single household
pvGenerationKernel
  :: Exp GPUPVSpec
  -> Exp GPUSimParams
  -> Exp GR              -- ^ Latitude
  -> Exp GR              -- ^ Longitude
  -> Exp GR              -- ^ Generated power (Watts)
pvGenerationKernel
  (GPUPVSpec_ arrAz arrTilt tempCorr power mountType modType)
  (GPUSimParams_ _ ambTemp windSpeed dayOfYear hourOfDay)
  lat
  lon =
    let
      effIrr = effectiveIrradianceKernel lat lon arrAz arrTilt dayOfYear hourOfDay
      modT = moduleTemp mountType modType effIrr ambTemp windSpeed
      cellT = cellTempKernel modT effIrr
      pvPower = maxPowerPointKernel power effIrr cellT tempCorr
    in
      A.max 0 pvPower

-- | Consumption calculation kernel
-- Calculates total consumption based on load states
consumptionKernel :: Exp GPULoadState -> Exp GR
consumptionKernel (GPULoadState_ (GPULoad_ power _) isRunning) =
  cond (isRunning A.== 1) power 0

-- | Update load states kernel (stochastic update simulation)
-- Uses time-based pseudo-random selection
updateLoadStatesKernel
  :: Exp GR              -- ^ Simulation time (for pseudo-randomness)
  -> Exp Int             -- ^ Node index (for variation)
  -> Exp GPULoadState
  -> Exp GPULoadState
updateLoadStatesKernel simTime nodeIdx (GPULoadState_ load@(GPULoad_ _ utility) _) =
  let
    -- Simple pseudo-random based on time and node
    pseudoRand = A.sin (simTime * A.fromIntegral nodeIdx * 12.9898) * 43758.5453
    randVal = pseudoRand - A.fromIntegral (A.floor pseudoRand :: Exp Int)
    newRunning = cond (randVal A.< utility) 1 0
  in
    GPULoadState_ load newRunning

-- | Single household step kernel
-- Computes the next state for one household
householdStepKernel
  :: Exp GPUHHSpec
  -> Exp GPUSimParams
  -> Exp GPUHHState
  -> Exp GPUHHState
householdStepKernel
  (GPUHHSpec_ nodeId lat lon gridX gridY battery pv)
  simParams@(GPUSimParams_ deltaT ambTemp windSpeed dayOfYear hourOfDay)
  (GPUHHState_ battState _ consumption txIn txOut) =
    let
      -- PV generation
      generation = pvGenerationKernel pv simParams lat lon

      -- Net power (generation - consumption + grid import - grid export)
      netPower = generation - consumption + txIn - txOut

      -- Battery current (positive = charging)
      battVoltage = case battState of
        GPUBatteryState_ v _ _ _ _ -> v
      batteryCurrent = cond (battVoltage A.> 0)
        (netPower / battVoltage)
        0

      -- Battery state update
      newBattState = batteryStepKernel battery deltaT batteryCurrent battState

      -- For now, simple grid balancing (export excess, import deficit)
      newTxIn = cond (netPower A.< 0) (A.abs netPower) 0
      newTxOut = cond (netPower A.> 0) netPower 0
    in
      GPUHHState_ newBattState generation consumption newTxIn newTxOut

-- | Vectorized grid step kernel
-- Processes all households in parallel on GPU
gridStepKernel
  :: Acc (Vector GPUHHSpec)    -- ^ Household specifications
  -> Acc (Scalar GPUSimParams) -- ^ Simulation parameters
  -> Acc (Vector GPUHHState)   -- ^ Current states
  -> Acc (Vector GPUHHState)   -- ^ Next states
gridStepKernel specs simParamsScalar states =
  A.zipWith (householdStepKernel' sp) specs states
  where
    sp = the simParamsScalar
    householdStepKernel' params spec state = householdStepKernel spec params state

-- | Aggregate rewards kernel
-- Computes total reward across all households
aggregateRewardsKernel :: Acc (Vector GPUHHState) -> Acc (Scalar GR)
aggregateRewardsKernel states = A.fold (+) 0 rewards
  where
    rewards = A.map rewardFromState states
    rewardFromState (GPUHHState_ (GPUBatteryState_ _ _ energy _ _) gen con _ _) =
      energy + gen - con

-- | Initialize battery states kernel
-- Creates initial states for all batteries
initBatteryStatesKernel :: Acc (Vector GPUBatterySpec) -> Acc (Vector GPUBatteryState)
initBatteryStatesKernel = A.map initBatteryState
  where
    initBatteryState (GPUBatterySpec_ _ _ totalCap _ _ vNom _ _ _ _ _ _) =
      let
        initialSoC = 50.0  -- Start at 50% charge
        initialV = vNom * (initialSoC / 100)
        initialE = totalCap * vNom * 0.5
      in
        GPUBatteryState_ initialV initialSoC initialE 0 0

-- | Initialize household states kernel
initHHStatesKernel :: Acc (Vector GPUHHSpec) -> Acc (Vector GPUHHState)
initHHStatesKernel specs = A.map initHHState specs
  where
    initHHState (GPUHHSpec_ _ _ _ _ _ battery _) =
      let
        initBatt = initBatteryState battery
      in
        GPUHHState_ initBatt 0 0 0 0
    initBatteryState (GPUBatterySpec_ _ _ totalCap _ _ vNom _ _ _ _ _ _) =
      let
        initialSoC = 50.0
        initialV = vNom * (initialSoC / 100)
        initialE = totalCap * vNom * 0.5
      in
        GPUBatteryState_ initialV initialSoC initialE 0 0

--------------------------------------------------------------------------------
-- Vectorized Kernels (PufferLib-style)
-- These operate on [num_envs, num_nodes] shaped arrays
--------------------------------------------------------------------------------

-- | Vectorized grid step kernel
-- Processes ALL environments × ALL nodes in a single kernel launch
-- Shape: states [num_envs, num_nodes], params [num_envs]
vecGridStepKernel
  :: Acc (Array DIM2 GPUHHSpec)     -- ^ Specs: [num_envs, num_nodes]
  -> Acc (Vector GPUSimParams)      -- ^ Params: [num_envs]
  -> Acc (Array DIM2 GPUHHState)    -- ^ States: [num_envs, num_nodes]
  -> Acc (Array DIM2 GR)            -- ^ Actions: [num_envs, num_nodes] (e.g., discharge rates)
  -> Acc (Array DIM2 GPUHHState)    -- ^ Next states: [num_envs, num_nodes]
vecGridStepKernel specs params states actions =
  A.generate (A.shape states) stepAt
  where
    stepAt ix =
      let
        Z :. envIdx :. nodeIdx = unlift ix :: Z :. Exp Int :. Exp Int
        spec = specs A.! ix
        state = states A.! ix
        action = actions A.! ix
        param = params A.! (A.index1 envIdx)
      in
        householdStepWithAction spec param state action

-- | Household step with action input
-- Action modifies the battery charge/discharge behavior
householdStepWithAction
  :: Exp GPUHHSpec
  -> Exp GPUSimParams
  -> Exp GPUHHState
  -> Exp GR                        -- ^ Action: battery discharge rate [-1, 1]
  -> Exp GPUHHState
householdStepWithAction
  (GPUHHSpec_ nodeId lat lon gridX gridY battery pv)
  simParams@(GPUSimParams_ deltaT ambTemp windSpeed dayOfYear hourOfDay)
  (GPUHHState_ battState _ consumption txIn txOut)
  action =
    let
      -- PV generation
      generation = pvGenerationKernel pv simParams lat lon

      -- Action scales battery usage: -1 = max charge, +1 = max discharge
      battVoltage = case battState of
        GPUBatteryState_ v _ _ _ _ -> v
      maxDischargePower = case battState of
        GPUBatteryState_ _ _ _ _ dp -> dp
      maxChargePower = case battState of
        GPUBatteryState_ _ _ _ cp _ -> cp

      -- Apply action to determine battery current
      actionPower = cond (action A.>= 0)
        (action * maxDischargePower)
        (action * maxChargePower)

      batteryCurrent = cond (battVoltage A.> 0)
        (actionPower / battVoltage)
        0

      -- Battery state update
      newBattState = batteryStepKernel battery deltaT batteryCurrent battState

      -- Net power balance
      batteryPowerOut = actionPower
      netPower = generation - consumption - batteryPowerOut

      -- Grid exchange based on net power
      newTxIn = cond (netPower A.< 0) (A.abs netPower) 0
      newTxOut = cond (netPower A.> 0) netPower 0
    in
      GPUHHState_ newBattState generation consumption newTxIn newTxOut

-- | Initialize states for all environments
-- Shape: [num_envs, num_nodes]
vecInitStatesKernel
  :: Acc (Array DIM2 GPUHHSpec)     -- ^ Specs: [num_envs, num_nodes]
  -> Acc (Array DIM2 GPUHHState)    -- ^ Initial states: [num_envs, num_nodes]
vecInitStatesKernel specs = A.map initHHState specs
  where
    initHHState (GPUHHSpec_ _ _ _ _ _ battery _) =
      let
        initBatt = initBattState battery
      in
        GPUHHState_ initBatt 0 0 0 0
    initBattState (GPUBatterySpec_ _ _ totalCap _ _ vNom _ _ _ _ _ _) =
      let
        initialSoC = 50.0
        initialV = vNom * (initialSoC / 100)
        initialE = totalCap * vNom * 0.5
      in
        GPUBatteryState_ initialV initialSoC initialE 0 0

-- | Reset done environments while preserving non-done ones
-- This is the auto-reset logic for vectorized envs
vecResetKernel
  :: Acc (Array DIM2 GPUHHSpec)     -- ^ Specs: [num_envs, num_nodes]
  -> Acc (Vector Int)               -- ^ Done flags: [num_envs] (1 = reset this env)
  -> Acc (Array DIM2 GPUHHState)    -- ^ Current states: [num_envs, num_nodes]
  -> Acc (Array DIM2 GPUHHState)    -- ^ States after reset: [num_envs, num_nodes]
vecResetKernel specs dones states =
  A.generate (A.shape states) resetAt
  where
    resetAt ix =
      let
        Z :. envIdx :. nodeIdx = unlift ix :: Z :. Exp Int :. Exp Int
        isDone = dones A.! (A.index1 envIdx)
        currentState = states A.! ix
        spec = specs A.! ix
        freshState = initHHState spec
      in
        cond (isDone A.== 1) freshState currentState

    initHHState (GPUHHSpec_ _ _ _ _ _ battery _) =
      GPUHHState_ (initBattState battery) 0 0 0 0

    initBattState (GPUBatterySpec_ _ _ totalCap _ _ vNom _ _ _ _ _ _) =
      let
        initialSoC = 50.0
        initialV = vNom * (initialSoC / 100)
        initialE = totalCap * vNom * 0.5
      in
        GPUBatteryState_ initialV initialSoC initialE 0 0

-- | Compute rewards for all environments
-- Returns per-env reward (summed across nodes) and per-node rewards
vecComputeRewardsKernel
  :: Acc (Array DIM2 GPUHHState)    -- ^ States: [num_envs, num_nodes]
  -> ( Acc (Vector GR)              -- ^ Per-env rewards: [num_envs]
     , Acc (Array DIM2 GR)          -- ^ Per-node rewards: [num_envs, num_nodes]
     )
vecComputeRewardsKernel states = (envRewards, nodeRewards)
  where
    -- Per-node rewards
    nodeRewards = A.map rewardFromState states

    rewardFromState (GPUHHState_ (GPUBatteryState_ _ _ energy _ _) gen con _ _) =
      -- Reward: energy stored + generation - consumption (encourage efficiency)
      energy * 0.01 + gen * 0.1 - con * 0.1

    -- Sum across nodes dimension to get per-env rewards
    Z :. numEnvs :. numNodes = unlift (A.shape states) :: Z :. Exp Int :. Exp Int

    -- Fold along the inner (node) dimension
    envRewards = A.fold (+) 0 nodeRewards

-- | Extract observations from states
-- Flattens relevant state info into observation vector
vecExtractObsKernel
  :: Acc (Array DIM2 GPUHHState)    -- ^ States: [num_envs, num_nodes]
  -> Exp Int                        -- ^ Observation dimension per node
  -> Acc (Array DIM2 GR)            -- ^ Observations: [num_envs, obs_dim]
vecExtractObsKernel states obsDimPerNode =
  A.generate (A.index2 numEnvs totalObsDim) extractObs
  where
    Z :. numEnvs :. numNodes = unlift (A.shape states) :: Z :. Exp Int :. Exp Int
    totalObsDim = numNodes * obsDimPerNode

    -- Each node contributes obsDimPerNode features to the observation
    -- Features: [battery_soc, battery_energy, generation, consumption, tx_in, tx_out]
    extractObs ix =
      let
        Z :. envIdx :. obsIdx = unlift ix :: Z :. Exp Int :. Exp Int
        nodeIdx = obsIdx `A.div` obsDimPerNode
        featureIdx = obsIdx `A.mod` obsDimPerNode
        state = states A.! A.index2 envIdx nodeIdx
        GPUHHState_ (GPUBatteryState_ _ soc energy _ _) gen con txIn txOut = state
      in
        -- Select feature based on index
        cond (featureIdx A.== 0) soc $
        cond (featureIdx A.== 1) energy $
        cond (featureIdx A.== 2) gen $
        cond (featureIdx A.== 3) con $
        cond (featureIdx A.== 4) txIn $
        txOut

-- | Check which environments are done (episode termination)
-- Done conditions: battery depleted, max steps reached, etc.
vecCheckDoneKernel
  :: Acc (Array DIM2 GPUHHState)    -- ^ States: [num_envs, num_nodes]
  -> Acc (Vector Int)               -- ^ Step counts: [num_envs]
  -> Exp Int                        -- ^ Max episode length
  -> ( Acc (Vector Int)             -- ^ Done flags: [num_envs]
     , Acc (Vector Int)             -- ^ Truncated flags: [num_envs]
     )
vecCheckDoneKernel states stepCounts maxSteps = (dones, truncated)
  where
    Z :. numEnvs :. numNodes = unlift (A.shape states) :: Z :. Exp Int :. Exp Int

    -- Check if any node in env has depleted battery (done condition)
    -- Min battery SoC across nodes for each env
    minSoC = A.fold1 A.min $ A.map extractSoC states
    extractSoC (GPUHHState_ (GPUBatteryState_ _ soc _ _ _) _ _ _ _) = soc

    -- Done if any battery SoC < 5%
    dones = A.zipWith checkDone minSoC stepCounts
    checkDone soc steps = cond (soc A.< 5.0) 1 0

    -- Truncated if max steps reached (not a true termination)
    truncated = A.map (\steps -> cond (steps A.>= maxSteps) 1 0) stepCounts

-- | Apply actions to modify consumption/generation targets
-- Actions are interpreted as battery control signals
vecApplyActionsKernel
  :: Acc (Array DIM2 GR)            -- ^ Actions: [num_envs, num_nodes] in [-1, 1]
  -> Acc (Array DIM2 GR)            -- ^ Clipped/normalized actions
vecApplyActionsKernel actions = A.map clipAction actions
  where
    clipAction a = A.max (-1) (A.min 1 a)

--------------------------------------------------------------------------------
-- Graph Message Passing Kernels
-- These implement GNN-style scatter/gather for power flow on grid topology
--------------------------------------------------------------------------------

-- | Gather node features from neighbors
-- Given edge list (src, dst) and node features, gather src features for each edge
gatherNodeFeatures
  :: Acc (Vector Int)               -- ^ Source node indices: [num_edges]
  -> Acc (Vector GR)                -- ^ Node features: [num_nodes]
  -> Acc (Vector GR)                -- ^ Edge features (gathered from src): [num_edges]
gatherNodeFeatures srcIndices nodeFeatures = gather srcIndices nodeFeatures

-- | Scatter-add edge messages to destination nodes
-- Aggregates messages from all incoming edges at each node
scatterAddMessages
  :: Exp Int                        -- ^ Number of nodes
  -> Acc (Vector Int)               -- ^ Destination node indices: [num_edges]
  -> Acc (Vector GR)                -- ^ Edge messages: [num_edges]
  -> Acc (Vector GR)                -- ^ Aggregated messages at nodes: [num_nodes]
scatterAddMessages numNodes dstIndices edgeMessages =
  permute (+) defaults indexMap edgeMessages
  where
    defaults = fill (index1 numNodes) 0
    indexMap ix = Just_ (index1 (dstIndices A.! ix))

-- | Compute power sent FROM source (before transmission losses)
-- Returns (sent_power, received_power) per edge
computePowerFlowWithLosses
  :: Acc (Vector Int)               -- ^ Source indices: [num_edges]
  -> Acc (Vector Int)               -- ^ Destination indices: [num_edges]
  -> Acc (Vector GR)                -- ^ Edge weights (transmission efficiency η): [num_edges]
  -> Acc (Vector GR)                -- ^ Node surplus/deficit: [num_nodes]
  -> ( Acc (Vector GR)              -- ^ Power SENT by source: [num_edges]
     , Acc (Vector GR)              -- ^ Power RECEIVED by dest: [num_edges]
     , Acc (Vector GR)              -- ^ Power LOST in transmission: [num_edges]
     )
computePowerFlowWithLosses srcIdx dstIdx efficiencies nodePower =
  (sentPower, receivedPower, lostPower)
  where
    srcPower = gather srcIdx nodePower
    dstPower = gather dstIdx nodePower

    -- Power sent = half the surplus (bidirectional balancing)
    sentPower = A.zipWith computeSent srcPower dstPower
    computeSent pSrc pDst =
      let surplus = pSrc - pDst
      in cond (surplus A.> 0) (surplus * 0.5) 0

    -- Power received = sent * efficiency
    receivedPower = A.zipWith (*) efficiencies sentPower

    -- Power lost = sent - received = sent * (1 - efficiency)
    lostPower = A.zipWith (-) sentPower receivedPower

-- | Single message passing round for grid power balancing
-- Returns updated power at each node after exchange
-- NOTE: Total system energy decreases by transmission losses
messagePassingRound
  :: Exp Int                        -- ^ Number of nodes
  -> Acc (Vector Int)               -- ^ Edge sources: [num_edges]
  -> Acc (Vector Int)               -- ^ Edge destinations: [num_edges]
  -> Acc (Vector GR)                -- ^ Edge weights (transmission efficiency): [num_edges]
  -> Acc (Vector GR)                -- ^ Current node power: [num_nodes]
  -> Acc (Vector GR)                -- ^ Updated node power: [num_nodes]
messagePassingRound numNodes srcIdx dstIdx weights nodePower =
  A.zipWith (+) nodePower netFlow
  where
    -- Compute flow with proper losses
    (sentFlows, receivedFlows, _lostFlows) =
      computePowerFlowWithLosses srcIdx dstIdx weights nodePower

    -- Incoming power (what destination actually receives)
    incoming = scatterAddMessages numNodes dstIdx receivedFlows

    -- Outgoing power (what source actually sends - the full amount)
    outgoing = scatterAddMessages numNodes srcIdx sentFlows

    -- Net = received - sent (sources lose more than destinations gain)
    netFlow = A.zipWith (-) incoming outgoing

-- | Vectorized message passing for multiple environments
-- Edge topology is shared across envs, but node states differ
-- NOTE: Properly models transmission losses (source loses more than dest gains)
--
-- Complexity: O(E + N) per environment, fully parallel across envs AND edges
-- Uses scatter/gather pattern - no per-node iteration over edges
vecMessagePassingKernel
  :: Exp Int                        -- ^ Number of nodes per env
  -> Acc (Vector Int)               -- ^ Edge sources: [num_edges] (shared topology)
  -> Acc (Vector Int)               -- ^ Edge destinations: [num_edges]
  -> Acc (Vector GR)                -- ^ Edge weights (transmission efficiency): [num_edges]
  -> Acc (Array DIM2 GR)            -- ^ Node power: [num_envs, num_nodes]
  -> Acc (Array DIM2 GR)            -- ^ Updated node power: [num_envs, num_nodes]
vecMessagePassingKernel numNodes srcIdx dstIdx efficiencies nodePowers =
  A.zipWith (+) nodePowers netFlows
  where
    Z :. numEnvs :. _ = unlift (A.shape nodePowers) :: Z :. Exp Int :. Exp Int
    numEdges = A.length srcIdx

    -- Step 1: GATHER source and dest power for each edge × each env
    -- Shape: [num_envs, num_edges]
    srcPowers = A.generate (index2 numEnvs numEdges) $ \ix ->
      let Z :. env :. e = unlift ix :: Z :. Exp Int :. Exp Int
          srcNode = srcIdx A.! index1 e
      in nodePowers A.! index2 env srcNode

    dstPowers = A.generate (index2 numEnvs numEdges) $ \ix ->
      let Z :. env :. e = unlift ix :: Z :. Exp Int :. Exp Int
          dstNode = dstIdx A.! index1 e
      in nodePowers A.! index2 env dstNode

    -- Step 2: COMPUTE edge flows (fully parallel over envs × edges)
    -- Shape: [num_envs, num_edges]
    sentFlows = A.zipWith computeSent srcPowers dstPowers
    computeSent pSrc pDst =
      let surplus = pSrc - pDst
      in cond (surplus A.> 0) (surplus * 0.5) 0

    -- Broadcast efficiency to [num_envs, num_edges]
    receivedFlows = A.generate (index2 numEnvs numEdges) $ \ix ->
      let Z :. env :. e = unlift ix :: Z :. Exp Int :. Exp Int
          eta = efficiencies A.! index1 e
          sent = sentFlows A.! ix
      in eta * sent

    -- Step 3: SCATTER-ADD to aggregate at nodes
    -- For each env, scatter to [num_nodes], stack into [num_envs, num_nodes]
    --
    -- We flatten to 1D for permute, using (env * numNodes + node) indexing
    -- Then reshape back to 2D

    totalNodes = numEnvs * numNodes

    -- Flatten sent/received flows to [num_envs * num_edges]
    sentFlat = A.flatten sentFlows
    receivedFlat = A.flatten receivedFlows

    -- Create destination indices for scatter: [num_envs * num_edges]
    -- For edge e in env i: dst index = i * numNodes + dstIdx[e]
    scatterDstIdx = A.generate (index1 (numEnvs * numEdges)) $ \ix ->
      let Z :. flatIdx = unlift ix :: Z :. Exp Int
          env = flatIdx `A.div` numEdges
          e = flatIdx `A.mod` numEdges
          dstNode = dstIdx A.! index1 e
      in env * numNodes + dstNode

    scatterSrcIdx = A.generate (index1 (numEnvs * numEdges)) $ \ix ->
      let Z :. flatIdx = unlift ix :: Z :. Exp Int
          env = flatIdx `A.div` numEdges
          e = flatIdx `A.mod` numEdges
          srcNode = srcIdx A.! index1 e
      in env * numNodes + srcNode

    -- Scatter-add: O(E) atomic adds, fully parallel
    incomingFlat = permute (+) (fill (index1 totalNodes) 0)
                     (\ix -> Just_ (index1 (scatterDstIdx A.! ix)))
                     receivedFlat

    outgoingFlat = permute (+) (fill (index1 totalNodes) 0)
                     (\ix -> Just_ (index1 (scatterSrcIdx A.! ix)))
                     sentFlat

    -- Reshape back to [num_envs, num_nodes]
    incoming = A.reshape (index2 numEnvs numNodes) incomingFlat
    outgoing = A.reshape (index2 numEnvs numNodes) outgoingFlat

    -- Net flow = received - sent
    netFlows = A.zipWith (-) incoming outgoing

-- | Apply message passing to update household states with power exchange
vecApplyPowerExchange
  :: Exp Int                        -- ^ Number of nodes
  -> Acc (Vector Int)               -- ^ Edge sources
  -> Acc (Vector Int)               -- ^ Edge destinations
  -> Acc (Vector GR)                -- ^ Edge weights
  -> Acc (Array DIM2 GPUHHState)    -- ^ Current states: [num_envs, num_nodes]
  -> Acc (Array DIM2 GPUHHState)    -- ^ States with updated txIn/txOut
vecApplyPowerExchange numNodes srcIdx dstIdx weights states =
  A.zipWith updateState states netFlows
  where
    -- Extract net power (generation - consumption) from each node
    nodePowers = A.map extractNetPower states
    extractNetPower (GPUHHState_ _ gen con _ _) = gen - con

    -- Run message passing
    updatedPowers = vecMessagePassingKernel numNodes srcIdx dstIdx weights nodePowers
    netFlows = A.zipWith (-) updatedPowers nodePowers

    -- Update txIn/txOut based on net flow
    updateState (GPUHHState_ batt gen con _ _) netFlow =
      let
        txIn' = cond (netFlow A.> 0) netFlow 0
        txOut' = cond (netFlow A.< 0) (A.abs netFlow) 0
      in
        GPUHHState_ batt gen con txIn' txOut'
