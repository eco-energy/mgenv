{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE RankNTypes, TypeApplications, TypeOperators, ScopedTypeVariables, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
module Grid.Grid where

import Prelude hiding ((.), id, curry, uncurry, const)
import qualified ConCat.Category
import ConCat.AltCat


-- For Grid Instances
import GHC.Generics (U1(..), (:*:)(..), Par1(..), (:.:)(..), Generic)
import ConCat.Free.VectorSpace (V, HasV)
import ConCat.Free.LinearRow (L, HasL)


import LCirc.Cospan
import Algebra.Graph.Labelled

import GHC.Exts (Coercible, coerce)

import Control.Newtype.Generics
import ConCat.Misc ((:*), (:+), inNew, inNew2, R)


class Addressed a where
  asc :: a -> m Int

-- An injective type family
type family Grid e n i o = r | r -> e n i o

type instance Grid e n i o = Cospan (Graph e n) i o

--type instance Grid () () () () = ()


instance (Monoid e, Addressed n) => Category (Cospan (Graph e n))

#define NewtypeGrid(edge,node) type instance Grid (edge) =  Grid (O (node))

--NewtypeGrid(Par1 a a, Par1 a)
--NewtypeGrid(L s a b, s)

-- This is an endofunctor
data GF node edge i o = GF
  { unGF
    :: Cospan (Graph edge node) i o
  } deriving (Generic)

instance (Monoid e) => Newtype (GF e n i o)


class (HasL R e, HasL R n) => OkGF' e n i

instance (Monoid e, Addressed n) => Category (GF e n) where
  type Ok (GF e n) = OkGF' e n
  id = id
  (.) = inNew2 (.)
  --(GF a) . (GF b) = GF (a . b)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

--newtype Grid e n i o = Grid { unGrid ::  }


--mkGrid :: forall e n i o. (Monoid e) => Cospan i o (Graph e n) -> Grid e n i o
--mkGrid = Grid


--instance Category (Grid e n) where
--  id = inNewType id
