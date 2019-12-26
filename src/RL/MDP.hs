module RL.MDP where

{--
import Grid (GridState, GridAction, Reward, GridSpec, TrajectoryBuffer, generateGrid, initGridState, powerBalance, energyAllocation, gridStep)

import Control.Monad.Representable.State

class (Monad m, Functor t) => MDP m t s a r p0 where
  init :: p0 -> m s
  step :: s -> a -> m s
  reward :: s a s -> r
  reset :: p0 -> m s
  

instance MDP TrajectoryBuffer GridState GridAction Reward GridSpec where
  init (GridSpec g) = do
    grid <- generateGrid g
    state <- initGridState grid
    return state

  step (GridState g) (GridAction as) = as <$> g

  reward s a s' = energyAllocation s s' - powerBalance s' 
  
  reset = init
  

class Controller s a pol val where
  action :: pol -> val -> s -> a
  learnPolicy :: pol -> traj -> pol
  learnValue :: val -> traj -> val


runMDP GridSpec {..} = do
  gS <- init g
  
  runState gridStep gS 

--}
