{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Scan where

import Prelude as P

import Control.Arrow (first, second, (&&&))
import Control.Applicative (liftA2)

-- A Monitor is over a hypergraph of node streams. 

-- Each Node is a stream of EnergyStates
-- We compute the potentials at each node with every new measurement.
-- We regress to an SoC estimation every 30 minutes over the question of the battery's sensed output voltage.
-- We then regress to a distribution of deltaVbPerT
-- module Monitor where


-- A Monitor is over a hypergraph of node streams. 

-- Each Node is a stream of EnergyStates
-- We compute the potentials at each node with every new measurement.
-- We regress to an SoC estimation every 30 minutes over the question of the battery's sensed output voltage.
-- We then regress to a distribution of deltaVbPerT


-- what is a hypergraph

-- We need a parallel scan over a hypergraph. 

class Scan f where
  prefixScan, suffixScan :: Monoid m => f m -> (m, f m)

-- Given a structure of values, prefix and suffix scan methods will generate overall fold (of type m) plus a structure of the same type as the input.

-- We can scan via a conversion to and from the basic set of 5 functor combinators

-- 5 basic functor combinators

-- The Constant Functor
newtype Const x a = Const x deriving (Functor)

instance Scan (Const x) where
  prefixScan (Const x) = (mempty, Const x)
  suffixScan           = prefixScan


-- The Identity Functor
newtype Id a = Id a deriving (Functor)

instance Scan Id where
  prefixScan (Id m) = (m, Id mempty)
  suffixScan        = prefixScan


-- The Sum Functor
data (f :+: g) a = InL (f a) | InR (g a) deriving (Functor)


-- the instance test is: prefixScan InL === second InL . prefixScan
instance (Scan f, Scan g) => Scan (f :+: g) where
  prefixScan (InL fa) = second InL (prefixScan fa)
  prefixScan (InR ga) = second InR (prefixScan ga)

  suffixScan (InL fa) = second InL (suffixScan fa)
  suffixScan (InR ga) = second InR (suffixScan ga)


-- The Product Functor
data (f :*: g) a = f a :*: g a deriving (Functor)

-- Scan each of the two parts separately, then combine the final fold part of one result with each
-- of the non-final elements of the other
instance (Scan f, Scan g, Functor f, Functor g) => Scan (f :*: g) where
  prefixScan (fa :*: ga) = (af <> ag, fa' :*: ((af <>) <$> ga'))
    where
      (af, fa') = prefixScan fa
      (ag, ga') = prefixScan ga
      
  suffixScan (fa :*: ga) = (af <> ag, fa' :*: ((af <>) <$> ga'))
    where
      (af, fa') = suffixScan fa
      (ag, ga') = suffixScan ga

-- The Composition Functor

newtype (g :. f) a = O { unO :: (g (f a)) } deriving (Functor)

-- to find the definitions, fiddle with types beginning at the domain type and arriving at the range type.
-- use these helpers

zip' :: Applicative g => (g a, g b) -> g (a, b)
zip' = uncurry (liftA2 (,))

unzip' :: Functor g => g (a, b) -> (g a, g b)
unzip' = fmap fst &&& fmap snd

assocR :: ((a, b), c) -> (a, (b, c))
assocR ((a, b), c) = (a, (b, c))

adjustL :: (Functor f, Monoid m) => (m, f m) -> f m
adjustL (m, ms) = (m <>) <$> ms

adjustR :: (Functor f, Monoid m) => (m, f m) -> f m
adjustR (m, ms) = (<> m) <$> ms

instance (Scan g, Scan f, Functor f, Applicative g) => Scan (g :. f) where
  prefixScan = second (O . fmap adjustL . zip')
               . assocR
               . first prefixScan
               . unzip'
               . fmap prefixScan
               . unO
  suffixScan = second (O . fmap adjustR . zip')
               . assocR
               . first suffixScan
               . unzip'
               . fmap suffixScan
               . unO
  
-- prefixScan, suffixScan ∷ Monoid m => (g :. f) m -> (m, (g :.f) m)
-- Encoding and Decoding, Scanning Implementation for lists

class EncodeF f where
  type Enc f :: * -> *
  encode :: f a -> Enc f a
  decode :: Enc f a -> f a

prefixScanEnc, suffixScanEnc :: (EncodeF f, Scan (Enc f), Monoid m) => f m -> (m, f m)
prefixScanEnc = second decode . prefixScan . encode
suffixScanEnc = second decode . suffixScan . encode

instance EncodeF [] where
  type Enc [] = Const () :+: (Id :*: [])
  encode [] = InL (Const ())
  encode (a : as) = InR (Id a :*: as)
  decode (InL (Const ())) = []
  decode (InR (Id a :*: as)) = a : as

instance Scan [] where
  prefixScan = prefixScanEnc
  suffixScan = suffixScanEnc

data Pair a = a :# a deriving (Eq, Ord, Show, Functor)

mkPair :: (a, a) -> Pair a
mkPair (a, b) = a :# b

instance EncodeF Pair where
  type Enc Pair = Id :*: Id
  encode (a :# b) = Id a :*: Id b
  decode (Id a :*: Id b) = a :# b

instance Scan Pair where
  prefixScan = prefixScanEnc
  suffixScan = suffixScanEnc

-- Bottom-up Trees for putting scanable things in
data BT a = Leaf a | Branch (BT (Pair a)) deriving (Functor)

instance Applicative BT where
  pure = Leaf
  Leaf f <*> Leaf x = Leaf (f x)
  Branch fgs <*> Branch xys = Branch (liftA2 h fgs xys)
    where
      h (f :# g) (x :# y) = f x :# g y
  _ <*> _ = error "BT <*>: structure mismatch"

instance EncodeF BT where
  type Enc BT = Id :+: (BT :. Pair)
  encode (Leaf a) = InL (Id a)
  encode (Branch t) = InR (O t)
  decode (InL (Id a)) = Leaf a
  decode (InR (O t)) = Branch t

instance Scan BT where
  prefixScan = prefixScanEnc
  suffixScan = suffixScanEnc

{--
type R = Double

type AdjacencyGraph n = Map.Map n [n] 

type NodeState = S 

newtype HyperGraph x = HyperGraph { hg :: ([x], [[x]]) } deriving (Eq, Ord, Show, Generic)

potentialEnergy :: HyperGraph NodeState -> R
potentialEnergy hg = (uncurry (-)) (zip stored load)
  where
    stored = P.map (toEnergy . (\n-> (batteryCurrents n, batteryVoltage n))) hg
    load = P.map (toEnergy . (\n-> (loadCurrents n, batteryVoltage n))) hg
    toEnergy p t = p * t
    
--}    


{--

-----------------------------------------------------------------

-- Propagators

-- inv g <> g = mempty
-- g <> inv g = mempty
class Monoid g => Group g where
  inv :: g -> g

-- act mempty = id
-- act (m <> n) = act m . act n
class Group g => GroupAction g s where
  act :: g -> s -> s

newtype Delta = Delta Int
  deriving (Num, Show, Eq)

instance Semigroup Delta where
  (<>) = (+)

instance Monoid Delta where
  mempty = 0

instance Group Delta where
  inv = negate


-- relative is an example of a group action.
class Relative s where
  rel :: Delta -> s -> s

data Error = Error Delta String

instance Relative Error where
  rel d (Error d' s) = Error (d <> d') s

-- O(n)
instance Relative a => Relative [a] where
  rel = fmap . rel

data List a = Nil | Cons Delta a (List a)

instance Relative (List a) where
  rel 0 xs = xs
  rel d Nil = Nil
  rel d (Cons d' a as) = Cons (d <> d') a as


--}
