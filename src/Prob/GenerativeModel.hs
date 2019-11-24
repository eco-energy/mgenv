module Prob.GenerativeModel where

import Streamly.Prelude as S
import Streamly

{---
import Data.Functor.Rep
import ConCat.Distribution
import ConCat.Choice
import ConCat.Circuit
import ConCat.RunCircuit (run)
import ConCat.Deep (trainNTimes, err1Grad, err1, affRelu, affLog, affRelu, (@.), step,  )


{--
• Beginning with a simple category of derivative-augmented functions, specify AD simply and precisely by
requiring this augmentation (relative to regular functions) to be homomorphic with respect to a collection
of standard categorical abstractions and primitive mathematical operations.
• Calculate a correct-by-construction AD implementation from the homomorphic specification.
• Generalizing AD by replacing linear maps (general derivative values) with an arbitrary cartesian category
[Elliott, 2017], define several AD variations, all stemming from different representations of linear maps:
functions (satisfying linearity), “generalized matrices” (composed representable functors), continuation-based
transformations of any linear map representation, and dualized versions of any linear map representation.
The latter two variations yield correct-by-construction implementations of reverse-mode AD that are
much simpler than previously known and are composed from generally useful components. The choice
of dualized linear functions for gradient computations is particularly compelling in simplicity. It also
appears to be quite efficient—requiring no matrix-level representations or computations—and is suitable
for gradient-based optimization, e.g., for machine learning. In contrast to conventional reverse-mode
AD algorithms, all algorithms in this paper are free of mutation and hence naturally parallel. A similar
construction yields forward-mode AD.

--}


type R = Double

type SampleSpace = [R]

type GeneratingFunction = (SampleSpace -> R)

type Topic = Int


instance Representable Dist where
  type Rep Dist = Rep Dist
  index = undefined
  tablulate = reduction . evidence




maxProb :: (Representable a) => (a -> R) -> a -> R 
maxProb (Dist d)=

generate :: Topic -> IO ()
generate topic = do
  return maxProb $ distribution query conditioning =<< subscribe topic
  where
    maxProb :: (RepresentableFunctor) => Dist a -> Query a -> Space a -> 
    maxProb = undefined
    distribution ::  -> a0 -> Dist b0
    distribution = undefined
    query = undefined
    conditioning = undefined
    subscribe = undefined
    
--}
