{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, TypeApplications, TypeOperators, ScopedTypeVariables, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module Grid.Grid where

import Prelude hiding ((.), id, curry, uncurry, const)
import qualified ConCat.Category
import ConCat.AltCat


-- For Grid Instances
import GHC.Generics (U1(..), (:*:)(..), Par1(..), (:.:)(..), Generic)
import ConCat.Free.VectorSpace (V, HasV)
import ConCat.Free.LinearRow (L, HasL)


import LCirc.Cospan
import LCirc.LCirc hiding (VI)
import LCirc.Spider


import Algebra.Graph.Labelled

import GHC.Exts (Coercible, coerce)

import Control.Newtype.Generics
import ConCat.Misc ((:*), (:+), inNew, inNew2, R)

import Data.Monoid (Sum)


-- newtype VI k = VI (k :* k) deriving (Eq, Ord, Show, Generic)

-- instance (Num k) => Frobenius (VI k)

-- instance (Frobenius a) => Semigroup a where
--   (<>) = fmerge

-- instance (Frobenius a) => Monoid a where
--   mempty = funit ()

-- class Addressed a where
--   asc :: a -> m Int

-- -- An injective type family
-- type family Grid e n i o = r | r -> e n i o

-- type instance Grid e n i o = Cospan (Graph e n) i o

-- --type instance Grid () () () () = ()

-- {--
-- instance (Monoid e, Addressed n) => Category (Cospan (Graph e n)) where
--   id = id
--   (.) = pushout
-- --}
-- #define NewtypeGrid(edge,node) type instance Grid (edge) =  Grid (O (node))

-- --NewtypeGrid(Par1 a a, Par1 a)
-- --NewtypeGrid(L s a b, s)

-- -- This is an endofunctor
-- data GF edge node i o = GF
--   { unGF
--     :: Cospan (Graph edge node) i o
--   } deriving (Generic)

-- instance (Monoid e) => Newtype (GF e n i o)


-- class (HasL R e, HasL R n, HF i, HF o) => OkGF' e n i o


-- instance (Eq e, Monoid e, Ord n, Ord e) => Category (GF e n) where
--   type Ok (GF e n) = HF
--   id = id
--   (.) = inNew2 (.)
--   --(GF a) . (GF b) = GF (a . b)
--   {-# INLINE id #-}
--   {-# INLINE (.) #-}

-- type VIGraph = GF (VI R) Int

-- type EnergyGraph = GF (Sum R) Int

--attach :: VIGraph i oi -> VIGraph oi o -> VIGraph i o
--attach a b = b . a

-- USE THE BIFUNCTOR INSTANCE FOR GRAPH TO JUMP BETWEEN CATEGORIES
--newtype Grid e n i o = Grid { unGrid ::  }


--mkGrid :: forall e n i o. (Monoid e) => Cospan i o (Graph e n) -> Grid e n i o
--mkGrid = Grid


--instance Category (Grid e n) where
--  id = inNewType id
