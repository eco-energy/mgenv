{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RebindableSyntax #-}

-- | GPU computation kernels for parallel microgrid simulation
-- This module contains the core CUDA kernels that execute on the GPU
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
    -- * Grid-level Kernels
  , gridStepKernel
  , aggregateRewardsKernel
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

      -- Voltage update (simplified linear model)
      vtNext = vNom * (ztNext / 100)

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
