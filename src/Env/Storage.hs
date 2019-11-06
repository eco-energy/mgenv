{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Env.Storage where

import Data.Tuple.Extra
import Streamly.Prelude as S
import Streamly
import ConCat.Choice
import Elm (Elm, elmStreetParseJson, elmStreetToJson)


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

type R = Double

type V = R
type Ws = R
type Amp = R
type W = R
type Sec = Integer
type DelT = Int
type Q = Amp S
type SoC = R

type Efficiency = (R->R)

type ChargeEfficiency = CEfficiency
type DischargeEfficiecny = DEfficiency
type Eff = (ChargeEfficiency, DischargeEfficiency)

{--

Run an autoregressive representation of each battery parameter.
Maintain a fold over all the batteries as the net grid energy G_e.
Optimize for sum (netEnergy) - FearOfEOut * (all (each netEnergy n t > 0 forall n, t)) 

--}


data BatteryObservation = Load
                          { i_t :: Amp
                          , v_t :: V
                          , duration :: DelT }
                        | Charge
                          { i_t :: Amp
                          , v_t :: V
                          , duration :: DelT }
                        deriving (Eq, Show, Generic)

data BatteryState = BatteryState
  { z_t :: Dist (Map Cells Q)
  , e_t :: Dist WattHours
  , cp_t :: Dist Watts
  , dp_t :: Dist Watts
  } deriving (Eq, Show, Generic)
    deriving anyclass (Elm)



data BatteryParameters = BatteryParameters
  { coloumbicEff :: Eff
  , del_t :: DelT
  , totalChargeCapacity :: Q
  , qMin :: Q  -- depth of discharge minimum SOC
  , qMax :: Q  -- depth of discharge maximum SOC
  , vNominal :: V
  , vMin :: V
  , vMax :: V
  , delVDisAtI :: (Amp -> V)
  , iDis :: Amp
  , delVChgAtI :: (Amp -> V)
  , iChg :: Amp
  } deriving (Eq, Show, Generic)
    deriving anyclass (Elm)


stateNext :: BatteryParameters -> BatteryState -> BatteryObservation -> BatteryState
stateNext BatteryParameters {..} BatteryState {..} BatteryObservation {..} =
  BatteryState $ ztNext etNext ptNext
  where
    ztNext = z_t - (del_t / Q)  - (ce * i_t)
      where
        ce = if (i_t <= 0) then fst coloumbicEff else snd coloumbicEff
    etNext = totalChargeCapacity * vNominal * (zt_next - (qMin / totalChargeCapacity))
    ptNext = (Discharge dp, Charge cp)
      where
        dp = p vMin rDis
        dc = p vMax rChg
        p vRef r = vRef * ((ocv z_t - vRef) / r) 
        ik = ((ocv ztNext) - vMin / rK)
        rDis = delVDisAtI / iDis
        rChg = delVChgAtI / iChg


cyclesCompleted :: BatteryState -> (ChargeHours, DischargeHours)
cyclesCompleted (BatteryState zk) = len zk


data ChargeEvent = CCCV {}
                 | CV {}
                 deriving (Eq, Ord, Generic) deriving anyclass (Elm)

data DischargeEvent = Scheduled | Unscheduled deriving (Eq, Ord, Generic)








-- Instances for elm type generation
instance ToJSON BatteryObservation where toJSON = elmStreetToJson
instance FromJSON BatteryObservation where fromJOSN = elmStreetFromJSON

instance ToJSON BatteryState where toJSON = elmStreetToJson
instance FromJSON BatteryState where fromJOSN = elmStreetFromJSON

instance ToJSON BatteryParameters where toJSON = elmStreetToJson
instance FromJSON BatteryParameters where fromJOSN = elmStreetFromJSON

instance ToJSON ChargeEvent where toJSON = elmStreetToJSON
instance FromJSON ChargeEvent where fromJSON = elmStreetFromJSON




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


batterySoC :: BatteryParameters -> (BatteryState -> SoC)
batterySoC bp = stateOfCharge bp 

energyEstimate :: (Storage b) -> Wh
energyEstimate b = undefined

chargePower :: (Storage b) -> W
chargePower b = undefined

dischargePower :: (Storage b) -> W
dischargePower b = undefined



charge :: DelT ->  ChargeType -> BatteryState -> ET -> BatteryState
charge delT (CCCV ct) BatteryState b{..} ET e{..} = b <> toStorageDiff 
  where
    toStorageDiff :: (ET -> BatteryState) -> ET -> BatteryState
    
  
  
    pMin = energy / tLength
    voltage = 
    current = chargePower b
    
  in 

class LinearConstraint a where
  quadraticSolver :: a -> Action a

  
class Storage a where
  powerDist :: (LinearConstraint a) => Graph Dist a -> a
  energyDist :: (LinearConstraint a) => Graph Dist a -> a
  toStorageDiff (a -> BatteryState)
  


runBatteryStep Battery {BatteryState s, BatteryParameters p} = do
  step <- (\(b, filer) -> if filter then charge b CCCV else charge b CV) =<< charge cccv
   


diffusionResistorCurrent :: Amp
diffustionResistorCurrent _ = 0


runStateOfCharge :: (Monad m) => BatteryParameters -> m Q -> Q
runStateOfCharge = undefined

runDischargePower :: (Monad m) => (ma -> a) -> ma -> a


data Battery = Battery { p :: BatteryParameters, s :: BatteryState, c :: T, }


{--
data Battery = LithiumIon
               { state :: BatteryState
               , lithiumConcentrationAt0 :: R
               , lithiumConcentrationAt100 :: R
               }
             | LeadAcid
               { state :: BatteryState }
             deriving (Eq, Ord, Show)
--}


class Storage b where
  socEstimate :: b -> Volts -> Amperes ->  WattHours
  cycles :: b -> Int
  capacity :: b -> WattHours
  charge :: b -> WattHours -> b
  discharge :: b -> WattHours -> b
  capacityAtCycle :: b -> Int -> b
  updateCapacity :: b -> b
  dodLimits :: b -> (WattHours, WattHours)



instance Storage Battery where
  voltageToEnergyStored (LithiumIon b) v = undefined
  voltageToEnergyStored (LeadAcid b) v = undefined
  cycles b = (cyclesCompleted . state) b
  capacity b = (currentCapacity . state) b
  charge (LithiumIon b) = undefined
  charge (LeadAcid b) = undefined
  discharge (LithiumIon b) = undefined
  discharge (LeadAcid b) = undefined
  capacityAtCycle (LithiumIon b) cyc = undefined
  capacityAtCycle (LeadAcid b) cyc = undefined
  dodLimits b = both (voltageToEnergyStored b) $ dodLimitsV . state $ b
  updateCapacity b{BatteryState {state}} = undefined


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


--}




{--
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

stateOfCharge (LithiumIon b) = undefined

constantPowerConstantVoltageCharge = undefined
constantVoltageCharge BatteryState { t_high, v_high }  


chargeBatteryAtInstant :: Battery -> Volts -> Amperes -> Battery
chargeBatteryAtInstant b v i = b

-- first mode is constant current, all power maximally fed to battery
-- second regime is constant power -- range : 13.5v to 14.5v
-- third regime is drip-charging

-- low-voltage c


