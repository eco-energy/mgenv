{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Physics.Storage (BatteryState(..), BatterySpec(..), sampleBatterySpec, stateNext, initBatteryState, batteryVoltage, Storage, energyStored) where

import Data.Tuple.Extra ()
import GHC.Generics hiding (R)
import Physics.Units
import Control.Monad.Bayes.Class
import Control.Monad (liftM)

import Randomizable
{--
In terms of the environment design, what the RL controller should see
must be just an estimate of the battery energy state in WattHours.
There is necessary complexity in how that's calculated for different kinds of
batteries, but in the beginning it's fine to just use a dumb linear model for everything.

In this case, the evolution of the battery state is governed by:
- charge :: Battery -> Current -> Voltage -> TimeDelta -> Battery
- discharge :: Battery -> Current -> Voltage -> TimeDelta -> Battery
- availableEnergy :: Battery -> WattHours
- chargePower :: Battery -> Watts
- dischargePower :: Battery -> Watts


The following Distributions are relevant:
 - energy :: Ampere -> Volt -> Time -> Energy
 - power :: Ampere -> Volt -> Power
 
 - powerIn :: power uncurry $ (Source -> (Ampere, Volt)) Source
 - powerOut :: power uncurry $ (Sink -> (Ampere, Volt)) Source

Here, the currents and voltages in the charge and discharge functions are 

Over the long-term, the total battery capacity degrades.
Would be nice to have this incorporated in the charge and discharge functions. 

--}

class Storage a where
  chargePower :: a -> Watts
  dischargePower :: a -> Watts
  energyStored :: a -> WattHours


type ChargeEfficiency = Efficiency
type DischargeEfficiency = Efficiency
type Eff = (ChargeEfficiency, DischargeEfficiency)

data BatteryObservation = BatteryObservation
                          { i_t :: Amp
                          , termV_t :: V
                          , duration :: DelT }
                        deriving (Eq, Show, Generic)

data BatteryState = BatteryState
  { v_t :: V
  , z_t :: SoC
  , e_t :: WattHours
  , cp_t :: Watts
  , dp_t :: Watts
  } deriving (Eq, Ord, Show, Generic)


data BatterySpec = BatterySpec
  { coloumbicEff :: Eff
  , totalChargeCapacity :: AmpH
  , qMin :: AmpH  -- depth of discharge minimum SOC
  , qMax :: AmpH  -- depth of discharge maximum SOC
  , vNominal :: V
  , vMin :: V
  , vMax :: V
  , delVDisAtI :: V -- should be (Amp -> V) with iDis as the first parameter
  , iDis :: Amp -- should be a parameter to delVDisAtI
  , delVChgAtI :: V -- same as above, should be (Amp -> V)
  , iChg :: Amp -- same as above
  } deriving (Eq, Show, Generic)


instance Randomizable BatterySpec where
  sampleThis = sampleBatterySpec

data Battery = Battery
               { params :: BatterySpec
               , state :: BatteryState
               , obs :: BatteryObservation
               } deriving (Eq, Show, Generic)


instance Storage BatteryState where
  chargePower BatteryState { .. } =  cp_t
  dischargePower BatteryState { .. } = dp_t
  energyStored BatteryState { .. } = e_t

sampleBatterySpec :: MonadSample m => m BatterySpec
sampleBatterySpec = do
  eff <- do
      c <- normal 0.8 0.2
      d <- normal 0.9 0.2
      return (c, d)
  cap <- (uniformD [40, 50.. 400])
  qMin <- liftM (*cap) $ uniform 0.2 0.5
  qMax <- liftM (*cap) $ uniform 0.8 0.99
  vNom <- normal 12 0.5
  vMax <- liftM ((+) vNom . abs) $ normal 2.0 1.0
  vMin <- liftM ((-) vNom . abs) $ normal 2.0 1.0
  dischargeDeltaV <- uniform 0.1 0.5
  dischargeRefCurr <- uniform 0.1 40
  chargeDeltaV <- uniform 0.1 0.5
  chargeRefCurr <- uniform 0.1 40
  return $ batterySpec eff cap qMin qMax vNom vMin vMax dischargeDeltaV dischargeRefCurr chargeDeltaV chargeRefCurr


stateNext :: BatterySpec -> BatteryState -> Amp -> DelT -> BatteryState
stateNext BatterySpec {..} BatteryState {..} current del_t = batteryState vt_next ztNext etNext dpNext cpNext
  where
    vt_next = v_t -- wrong
    ztNext :: SoC
    ztNext = z_t - (dt / (ahToColoumb totalChargeCapacity))  - (ce * current)
      where
        ce = if (current <= 0) then fst coloumbicEff else snd coloumbicEff
    etNext :: WattHours
    etNext = storedEnergy totalChargeCapacity vNominal ztNext qMin
    cpNext :: Watts
    cpNext = power vMax $ chargeCurrentLimit ocV vMax rChg
    dpNext :: Watts
    dpNext = power vMin $ dischargeCurrentLimit ocV vMin rDis
    rDis :: Ohm
    rDis = internalResistance delVDisAtI iDis
    rChg :: Ohm
    rChg = internalResistance delVChgAtI iChg
    ocV = vNominal
    dt = fromIntegral del_t

batteryVoltage :: BatteryState -> V
batteryVoltage = v_t

-- should always be greater than 0
storedEnergy :: AmpH -> V -> SoC -> AmpH -> WattHours
storedEnergy qmax vNom soc qmin = (qmax * vNom) * (soc - (qmin / qmax))

-- should always be greater than 0
internalResistance :: Fractional a => a -> a -> a
internalResistance deltaV refCurr = deltaV / refCurr

-- Should always be less than 0
chargeCurrentLimit :: V -> V -> Ohm -> Amp
chargeCurrentLimit ocV vMax chargeResistance = (ocV - vMax) / chargeResistance  

-- Should always be greater than 0
dischargeCurrentLimit :: V -> V -> Ohm -> Amp
dischargeCurrentLimit ocV vMin dischargeResistance = (ocV - vMin) / dischargeResistance

-- horrible, get a better model
ocvAtSoC :: BatterySpec -> SoC -> V
ocvAtSoC BatterySpec {..} soc = vNominal

power :: V -> Amp -> Watts
power v i = v * i


ahToColoumb :: AmpH -> Q
ahToColoumb ah = ah * 3600

coloumbToAh :: Q -> AmpH
coloumbToAh c = c / 3600

initBatteryState :: BatteryState
initBatteryState = BatteryState 0 0 0 0 0

defaultObs :: BatteryObservation
defaultObs = BatteryObservation 0 0 0

mkBattery :: Battery
mkBattery = Battery defaultParameters initBatteryState defaultObs

batterySpec :: Eff -> AmpH -> AmpH -> AmpH -> V -> V -> V -> V -> Amp -> V -> Amp -> BatterySpec
batterySpec = BatterySpec

toEff :: ChargeEfficiency -> DischargeEfficiency -> Eff
toEff c d = (c, d) :: Eff

batteryState :: V -> SoC -> WattHours -> Watts -> Watts -> BatteryState
batteryState = BatteryState

defaultParameters :: BatterySpec
defaultParameters = BatterySpec eff totCap qMin qMax vNom vMin vMax ddisVAtI iDis dchgVAtI iChg
  where
    eff = (0.85, 0.99) :: Eff
    totCap = 120 :: AmpH
    qMin = totCap * 0.5 :: AmpH
    qMax = totCap * 0.9 :: AmpH
    vNom = 12.3 :: V
    vMin = 11.3 :: V
    vMax = 14.3 :: V
    ddisVAtI = 0.1 :: V -- 0.1 volts discharge per 1/2 Amp of output power over a time t
    iDis = 0.5 :: Amp
    dchgVAtI = 0.1 :: V
    iChg = 0.5 :: Amp

{--
cyclesCompleted :: Battery -> Int
cyclesCompleted (Battery _ _ o) = length o


data ChargeEvent = CCCV {}
                 | CV {}
                 deriving (Eq, Ord, Generic)
                 deriving (Elm, ToJSON, FromJSON) via ElmStreet ChargeEvent

data DischargeEvent = Scheduled
                    | Unscheduled
                    deriving (Eq, Ord, Generic)
                    deriving (Elm, ToJSON, FromJSON) via ElmStreet DischargeEvent
--}



{--
charge :: Battery -> ChargeEvent -> Battery
charge b e = undefined

discharge :: Battery -> DischargeEvent -> Battery
discharge b l = undefined
--}

{--

Run an autoregressive representation of each battery parameter.
Maintain a fold over all the batteries as the net grid energy G_e.
Optimize for sum (netEnergy) - FearOfEOut * (all (each netEnergy n t > 0 forall n, t)) 

--}


{--
## State of Charge Dependence
The voltage of a fully charged cell is higher than the voltage of a discharged cell when the cell is in unloaded equilibrium.
The state of charge (z) is 100% when the cell is fully charged and 0% when it is fully discharged. 
Total Charge Capacity (Q), and determines how much charge the battery has at any state of charge.

The relationship between the SoC and the total charge capacity is:
        dz(t)/dt = -n(t)*i(t) / Q
        where i(t) is positive when discharging, so discharging lowers the cell's SoC and charging increases it.
              i(t) is measured in amperes and Q is measured in ampere-seconds.
              n(t) is the coulombic efficiency or charge efficiency of the cell.
                   it can be modelled as 1 at t where i(t) <= 0 and <=1 where i(t) > 0 (reasonable for L-Ion but not others)
              coloubmic efficiency is not energy efficiency as it is (charge in)/(charge out) and not (energy in)/(energy out),
              which would require accounting for energy loss due to resistive heating and is generally 95% for L-ion. n is 99%.

Integration over the ODE gives us the state of charge at any time t > t0:
        z(t) = z(t0) - (1/Q) integral t0 t [n(tau) * i(tau)] dtau

The discrete time formulation is defined with respect to:
sample interval: del_t (seconds)
sample frequency: 1/del_t (hertz)
t0 = k*del_t and t = (k+1)*del_t
        z[k+1] = z[k] - (del_t / Q) n[k]i[k]

The revision to our OCV model as a result of this is that the
ideal voltage source is replaced by a controlled voltage source having value equal ti OCV(z(t)). With temperature dependence:
OCV(z(t), T(t))

-}



{--
class Storage b where
  socEstimate :: b -> Volts -> Amperes ->  WattHours
  cycles :: b -> Int
  capacity :: b -> WattHours
  charge :: b -> WattHours -> b
  discharge :: b -> WattHours -> b
  capacityAtCycle :: b -> Int -> b
  updateCapacity :: b -> b
  dodLimits :: b -> (WattHours, WattHours)
--}


{--
There are three kinds of models useful for batteries:
- Equivalent Circuit Models: Computationally Cheap but inaccurate
- Electrochemical Models : Computationally Complex but accurate
- Physics-based Dynamics: Computationally Palatable and accurate

# Equivalent Circuit Models:

Behavioural approximation to how a cell's voltage responds to different input-current stimuli

## Open-Circuit Voltage
This is the most fundamental observed behaviour of a cell: It delievers a voltage at the terminals.
The simplest model for a cell is thus an ideal voltage source emitting a constant terminal voltage v(t).
Open circuit here means that the cell is unloaded and in complete equilibrium.


A careful definition of the the state of charge:

A cell is at a fully charged state after a Constant Current charging step has brought
its Open Circuit Voltage to the manufacturer-specified V_h, and then a Constant
Voltage charging step has been performed until the charging current has become infinitesimal.

A cell is at a fully discharged state after a Constant Current discharge step has brought its
Open Circuit Voltage to the manufacturer-sepcified V_l, and then a Constant Voltage discharge step
has been performeed until the discharge current has become infinitesimal.

Total capacity is the quantity of charge that would be removed from
the cell if it were brought from a fully charged state to a fully discharged state. (Ah)

Discharge capacity is the quantity of charge removed from
the cell if it were brought from a fully charged state to a fully discharged state
at a constant discharge current. (Ah)

Residual capacity is the quantity of charge removed from the
cell if it were brought from its present state to a fully discharged state. (Ah)

Cell state-of-charge is the ratio of the residual capacity to total capacity:

z_k+1 = z_k - (n_k*i_k (delta t)) / Q

z(t) = z(0) - (1/Q)* integral_0_t n(t)i(t)dt

where
   Q = cell total capacity
   n_k = charge_discharge_efficiency

--}
