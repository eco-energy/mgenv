-- A Vocabulary for Gradient Estimation using Stochastic Computation Graphs

module SCG where

import Data.Pointed
import Data.Key
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..))

import ConCat.AltCat
import ConCat.Rebox ()

import ConCat.Mist ((:*), Yes1, result, sqr, unzip, cond)

import ConCat.RAD (andDerR, derD, andGradR, gradR, andDerRL, derRL, andGrad2R)


--newtype Theta a = Theta { unT :: a } deriving (Generic) 

-- Parameterized Probability Distribution
class (Num a, Num b, Rep a, Num b) => Dist a b where
  type Th a = Th { unTh :: a}
  -- a is the result type and b 
  logProb :: (Th a -> b) -> Th a -> a 
  sample :: Dist a b -> b -> a

newtype Expectation a = Expectation { unE :: a } deriving (Eq, Ord, Show, Generic)

-- score function estimator
-- gradR :: Num s => (a -> s) -> (a -> a)
-- derivative of the 
-- f must be a continuous function of theta
sf :: (Num x, Num th, Num y, Expectation e) => (x -> th -> e) -> Dist x th -> th -> e
sf f pd th x = expect $ f ^* (gradR logProb d)


-- pathwise derivative: applicable when differentiating f x
-- where x is a deterministic differentiable function of th and another random variable z
pd :: (Num x, Num y, Num th, Num z, Expectation e) => (x -> e) -> (z -> th -> x) -> Dist z zp -> th -> zp -> e
pd f x d th zp = (expect . gradR) f $ x z th
  where
    z = sample d zp

expect :: a -> Expectation a
expect = undefined

-- EQ (4)
-- gradEstPair :: ()

data Node = In | Det | Stoc | C

data SCG where
  Input :: a -> Input a
  Det :: a -> Det a
  Stoch :: a -> Stoch a
  Objective :: a -> Objective a
  Estimator :: a -> Estimator a

type SD = ()
type PD = 
gradEst :: Objective -> Estimator -> 
