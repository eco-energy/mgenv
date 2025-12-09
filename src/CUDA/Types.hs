{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | GPU-compatible data types for parallel CUDA simulations
--
-- This module defines data types that can be efficiently transferred to and
-- processed on GPU. It supports both single-environment and vectorized
-- (PufferLib-style) batched environment execution.
--
-- = Architecture
--
-- The vectorized types use 2D arrays with shape @[num_envs, num_nodes]@ to enable
-- massive parallelism across both environments and nodes simultaneously.
--
module CUDA.Types
  ( -- * GPU-Compatible Scalar Types
    GR
    -- * GPU Battery Types
  , GPUBatterySpec(..)
  , GPUBatteryState(..)
  , pattern GPUBatterySpec_
  , pattern GPUBatteryState_
    -- * GPU PV Types
  , GPUPVSpec(..)
  , pattern GPUPVSpec_
    -- * GPU Consumption Types
  , GPULoad(..)
  , GPULoadState(..)
  , pattern GPULoad_
  , pattern GPULoadState_
    -- * GPU Transmission Types
  , GPUTransmissionSpec(..)
  , pattern GPUTransmissionSpec_
    -- * GPU Household Types
  , GPUHHSpec(..)
  , GPUHHState(..)
  , pattern GPUHHSpec_
  , pattern GPUHHState_
    -- * GPU Grid Types (Single Environment)
  , GPUGridSpec(..)
  , GPUGridState(..)
    -- * Vectorized Environment Types (PufferLib-style)
  , VecEnvConfig(..)
  , VecEnvState(..)
  , StepResult(..)
  , ObsDim
  , ActionDim
    -- * Simulation Parameters
  , GPUSimParams(..)
  , pattern GPUSimParams_
    -- * Type Classes for GPU Conversion
  , ToGPU(..)
  , FromGPU(..)
  ) where

import Data.Array.Accelerate as A hiding (pattern V)
import qualified Data.Array.Accelerate as A
import GHC.Generics (Generic)

-- | GPU-compatible real number type (Double on GPU)
type GR = Double

-- | GPU-compatible battery specification
-- All fields are flattened for efficient GPU access
data GPUBatterySpec = GPUBatterySpec
  { gbsChargeEff       :: !GR    -- ^ Charge efficiency
  , gbsDischargeEff    :: !GR    -- ^ Discharge efficiency
  , gbsTotalCapacity   :: !GR    -- ^ Total charge capacity (AmpH)
  , gbsQMin            :: !GR    -- ^ Minimum DoD (AmpH)
  , gbsQMax            :: !GR    -- ^ Maximum DoD (AmpH)
  , gbsVNominal        :: !GR    -- ^ Nominal voltage
  , gbsVMin            :: !GR    -- ^ Minimum voltage
  , gbsVMax            :: !GR    -- ^ Maximum voltage
  , gbsDelVDisAtI      :: !GR    -- ^ Discharge voltage delta at reference current
  , gbsIDis            :: !GR    -- ^ Discharge reference current
  , gbsDelVChgAtI      :: !GR    -- ^ Charge voltage delta at reference current
  , gbsIChg            :: !GR    -- ^ Charge reference current
  } deriving (Generic, Show, Eq)

instance Elt GPUBatterySpec

pattern GPUBatterySpec_
  :: Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR
  -> Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR
  -> Exp GPUBatterySpec
pattern GPUBatterySpec_ ce de tc qn qx vn vmn vmx dvd id' dvc ic =
  Pattern (ce, de, tc, qn, qx, vn, vmn, vmx, dvd, id', dvc, ic)

-- | GPU-compatible battery state
data GPUBatteryState = GPUBatteryState
  { gbstVoltage       :: !GR    -- ^ Current voltage
  , gbstSoC           :: !GR    -- ^ State of charge
  , gbstEnergy        :: !GR    -- ^ Energy stored (WattHours)
  , gbstChargePower   :: !GR    -- ^ Charge power (Watts)
  , gbstDischargePower :: !GR   -- ^ Discharge power (Watts)
  } deriving (Generic, Show, Eq)

instance Elt GPUBatteryState

pattern GPUBatteryState_
  :: Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp GR
  -> Exp GPUBatteryState
pattern GPUBatteryState_ v soc e cp dp = Pattern (v, soc, e, cp, dp)

-- | GPU-compatible PV specification
data GPUPVSpec = GPUPVSpec
  { gpvsArrAzimuth     :: !GR    -- ^ Array azimuth angle
  , gpvsArrTilt        :: !GR    -- ^ Array tilt angle
  , gpvsTempCorrection :: !GR    -- ^ Temperature correction factor
  , gpvsPower          :: !GR    -- ^ Rated power (Watts)
  , gpvsMountType      :: !Int   -- ^ Mount type (0=OpenRack, 1=CloseRoof, 2=Insulated, 3=Tracker)
  , gpvsModuleType     :: !Int   -- ^ Module type (0=GlassCellGlass, 1=GlassCellPolymer, etc.)
  } deriving (Generic, Show, Eq)

instance Elt GPUPVSpec

pattern GPUPVSpec_
  :: Exp GR -> Exp GR -> Exp GR -> Exp GR -> Exp Int -> Exp Int
  -> Exp GPUPVSpec
pattern GPUPVSpec_ az tilt tc p mt modt = Pattern (az, tilt, tc, p, mt, modt)

-- | GPU-compatible load specification
data GPULoad = GPULoad
  { glPower   :: !GR    -- ^ Load power (Watts)
  , glUtility :: !GR    -- ^ Utility factor (probability of running)
  } deriving (Generic, Show, Eq)

instance Elt GPULoad

pattern GPULoad_ :: Exp GR -> Exp GR -> Exp GPULoad
pattern GPULoad_ p u = Pattern (p, u)

-- | GPU-compatible load state
data GPULoadState = GPULoadState
  { glsLoad      :: !GPULoad   -- ^ Load spec
  , glsIsRunning :: !Int       -- ^ 1 if running, 0 otherwise
  } deriving (Generic, Show, Eq)

instance Elt GPULoadState

pattern GPULoadState_ :: Exp GPULoad -> Exp Int -> Exp GPULoadState
pattern GPULoadState_ l r = Pattern (l, r)

-- | GPU-compatible transmission line specification
data GPUTransmissionSpec = GPUTransmissionSpec
  { gtsWireLength    :: !GR    -- ^ Wire length (Meters)
  , gtsCrossSection  :: !GR    -- ^ Cross section (MetersSq)
  , gtsResistivity   :: !GR    -- ^ Resistivity (OhmMeters)
  } deriving (Generic, Show, Eq)

instance Elt GPUTransmissionSpec

pattern GPUTransmissionSpec_
  :: Exp GR -> Exp GR -> Exp GR
  -> Exp GPUTransmissionSpec
pattern GPUTransmissionSpec_ wl cs r = Pattern (wl, cs, r)

-- | GPU-compatible household specification
-- Contains flattened data for all household components
data GPUHHSpec = GPUHHSpec
  { ghsNodeId      :: !Int           -- ^ Node identifier
  , ghsLatitude    :: !GR            -- ^ Geographic latitude
  , ghsLongitude   :: !GR            -- ^ Geographic longitude
  , ghsGridX       :: !GR            -- ^ Euclidean X coordinate
  , ghsGridY       :: !GR            -- ^ Euclidean Y coordinate
  , ghsBattery     :: !GPUBatterySpec   -- ^ Battery specification
  , ghsPV          :: !GPUPVSpec        -- ^ PV specification
  } deriving (Generic, Show, Eq)

instance Elt GPUHHSpec

pattern GPUHHSpec_
  :: Exp Int -> Exp GR -> Exp GR -> Exp GR -> Exp GR
  -> Exp GPUBatterySpec -> Exp GPUPVSpec
  -> Exp GPUHHSpec
pattern GPUHHSpec_ nid lat lon gx gy bat pv = Pattern (nid, lat, lon, gx, gy, bat, pv)

-- | GPU-compatible household state
data GPUHHState = GPUHHState
  { ghstBattery      :: !GPUBatteryState  -- ^ Battery state
  , ghstGeneration   :: !GR               -- ^ Current PV generation (Watts)
  , ghstConsumption  :: !GR               -- ^ Current consumption (Watts)
  , ghstTxIn         :: !GR               -- ^ Power received from grid (Watts)
  , ghstTxOut        :: !GR               -- ^ Power sent to grid (Watts)
  } deriving (Generic, Show, Eq)

instance Elt GPUHHState

pattern GPUHHState_
  :: Exp GPUBatteryState -> Exp GR -> Exp GR -> Exp GR -> Exp GR
  -> Exp GPUHHState
pattern GPUHHState_ bat gen con txin txout = Pattern (bat, gen, con, txin, txout)

-- | GPU grid specification - contains arrays of household specs
data GPUGridSpec = GPUGridSpec
  { ggsHouseholds   :: Vector GPUHHSpec           -- ^ Array of household specifications
  , ggsEdges        :: Vector (Int, Int)          -- ^ Edge list (node pairs)
  , ggsTransmission :: Vector GPUTransmissionSpec -- ^ Transmission specs for each edge
  } deriving (Show)

-- | GPU grid state - contains arrays of household states
data GPUGridState = GPUGridState
  { ggsStates       :: Vector GPUHHState   -- ^ Array of household states
  , ggsTime         :: Scalar GR           -- ^ Current simulation time (seconds since start)
  } deriving (Show)

-- | GPU simulation parameters
data GPUSimParams = GPUSimParams
  { gspDeltaT        :: !GR     -- ^ Time step (seconds)
  , gspAmbientTemp   :: !GR     -- ^ Ambient temperature
  , gspWindSpeed     :: !GR     -- ^ Wind speed (m/s)
  , gspDayOfYear     :: !Int    -- ^ Day of year (1-365)
  , gspHourOfDay     :: !GR     -- ^ Hour of day (0-24)
  } deriving (Generic, Show, Eq)

instance Elt GPUSimParams

pattern GPUSimParams_
  :: Exp GR -> Exp GR -> Exp GR -> Exp Int -> Exp GR
  -> Exp GPUSimParams
pattern GPUSimParams_ dt at ws doy hod = Pattern (dt, at, ws, doy, hod)

-- | Type class for converting CPU types to GPU types
class ToGPU cpu gpu where
  toGPU :: cpu -> gpu

-- | Type class for converting GPU types back to CPU types
class FromGPU gpu cpu where
  fromGPU :: gpu -> cpu

--------------------------------------------------------------------------------
-- Vectorized Environment Types (PufferLib-style)
--------------------------------------------------------------------------------

-- | Observation dimension - flattened observation size per node
type ObsDim = Int

-- | Action dimension - action size per node
type ActionDim = Int

-- | Configuration for vectorized environment
-- Specifies the parallelism dimensions and environment structure
data VecEnvConfig = VecEnvConfig
  { vecNumEnvs        :: !Int              -- ^ Number of parallel environments
  , vecNumNodes       :: !Int              -- ^ Nodes per environment (grid size)
  , vecObsDim         :: !Int              -- ^ Observation dimension per node
  , vecActionDim      :: !Int              -- ^ Action dimension per node
  , vecMaxEpisodeLen  :: !Int              -- ^ Maximum episode length (steps)
  , vecAutoReset      :: !Bool             -- ^ Auto-reset done environments
  , vecSharedTopology :: !Bool             -- ^ All envs share same grid topology
  } deriving (Eq, Show, Generic)

-- | Vectorized environment state
-- All arrays have shape [num_envs, ...] for batched processing
data VecEnvState = VecEnvState
  { -- | Household states: [num_envs, num_nodes]
    vesStates         :: !(Array DIM2 GPUHHState)
    -- | Household specs: [num_envs, num_nodes] or [1, num_nodes] if shared
  , vesSpecs          :: !(Array DIM2 GPUHHSpec)
    -- | Simulation time per env: [num_envs]
  , vesTimes          :: !(Vector GR)
    -- | Episode step count per env: [num_envs]
  , vesStepCounts     :: !(Vector Int)
    -- | Done flags per env: [num_envs] (1 = done, 0 = not done)
  , vesDones          :: !(Vector Int)
    -- | Truncated flags per env: [num_envs] (1 = truncated, 0 = not)
  , vesTruncated      :: !(Vector Int)
    -- | Cumulative reward per env: [num_envs]
  , vesCumulativeReward :: !(Vector GR)
    -- | Simulation parameters per env: [num_envs]
  , vesSimParams      :: !(Vector GPUSimParams)
  } deriving (Show)

-- | Result of a vectorized step operation
-- Standard RL interface: (obs, reward, done, truncated, info)
data StepResult = StepResult
  { -- | Observations: [num_envs, obs_dim]
    srObs             :: !(Array DIM2 GR)
    -- | Rewards: [num_envs]
  , srRewards         :: !(Vector GR)
    -- | Done flags: [num_envs]
  , srDones           :: !(Vector Int)
    -- | Truncated flags: [num_envs]
  , srTruncated       :: !(Vector Int)
    -- | Per-node rewards for detailed analysis: [num_envs, num_nodes]
  , srNodeRewards     :: !(Array DIM2 GR)
  } deriving (Show)
