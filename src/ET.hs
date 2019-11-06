module ET () where


import Env.Storage
{--
import Env.Gen
import Env.Load

import Model.AutoRegressive (Dist)

type R = Double

type NodeState = (Storage, Gen, Load, Demand)
--}

{--
data Grid = Grid
  { demand :: Graph (Dist Watt) 
  , energyStored :: Graph (Dist WattHour)
  , dischargePower :: Graph (Dist Watt)
  , chargePower :: Graph Dist (Dist Watt)
  , gen :: Graph (Dist Watt)
  , delta_t :: Int
  , policy :: Graph ObsVector -> Graph Dist ControlAction
  , action :: Dist ControlAction -> [ControlAction]
  } deriving (Eq, Ord, Show, Generic)

-- you can not have EnergyTransactions without a concept of Nodes.
-- you can not have nodes without a concept of sensors.



transact {ET ..} = do
  let
    nodeGap = ((stored + (delta_t * gen)) - demand)
   a <- (action . policy) toObsVector stored gen demand nodeGap
   return a
--}

{--
instance Serializable ET

data SVG = SVG {}

class Representable a where
  svgRep :: (a -> SVG)

instance Representable ET where
  svgRep (ET sender, reciever) = svgRep


instance Billable ET

instance Monitorable ET

instance PolicyFunction ET

instance ValueFunction ET

instance Rewarded ET
--}



class Env a where
  save ::(Serialable a, Serialized b) => (a -> b) -> a -> b
  retrieve :: (Serialized a, Serializable b) -> (a -> b) -> a -> b)
  subscribe :: (Serialized a, Monad m) => (m a -> NodeState) -> ma -> NodeState
  publish :: (Monad m) => NodeState -> m Control



instance Env ET where
  save = undefined
  retrive = undefined
  subscribe = undefined
  publish = undefined
