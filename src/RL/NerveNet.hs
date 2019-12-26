{-# Language OverloadedStrings #-}
{-# Language OverloadedLists #-}
{-# Language DeriveGeneric #-}
module RL.NerveNet where

import Algebra.Graph.Labelled
import TensorFlow.Core as TF
import TensorFlow.Minimize (minimizeWith, adam)
import TensorFlow.Ops ( placeholder, truncatedNormal, add, matMul
                      , relu, argMax, scalar, cast, reduceMean
                      , softmaxCrossEntropyWithLogits, equal, vector)
import Tensorflow.Session (runSession)
import Tensorflow.Variable (readValue, initializedVariable, Variable)
import Data.Text as T
import Data.Int (Int64)

import GHC.Generics (Generic)

data PolicySpec = PolicySpec
  { nameScope :: T.Text
  , inputSize :: Int
  , outputSize :: Int
  , obPlaceholder :: ()
  , trainable :: Bool
  , defineStd :: Bool
  , isBaseline :: Bool
  } deriving (Eq, Ord, Show, Generic)


buildNNLayer :: Int64 -> Int64 -> Tensor v Float -> Build (Variable Float, Variable Float, Tensor Build Float)
buildNNLayer inputSize outputSize input = do
  weights <- truncatedNormal (vector [inputSize, outputSize]) >>= initializedVariable
  
  
main = do
  withNameScope "PPO" undefined
