{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}


import ConCat.Isomorphism
import ConCat.AltCat

import ConCat.Rebox
import ConCat.Shaped

import qualified Data.Set as Set
import qualified Data.Map as Map


import ConCat.CircAff hiding (Cospan)

-- An implementation of Hypergraph categories.

-- Diagrams in Category C.

-- A commutative diagram A -f> B -h> D -i> C -j> A 

-- Algebraic constructions define operations on elements of a set, plus some laws that must be satisfied by these equations.

-- In Set, there is a general formula for the limit and colimit of any diagram I -D> Set:
    -- limit : limD = {(x1, x2, ... xi) | forall f : i -> i',  I}

infix 0 :>

infix 1 :<

data a :> b  = a :> b

data b :< c = b :< c

class (BraidedPCat a, MonoidalPCat a, MonoidalSCat a, BraidedSCat a, CoproductCat a) => Prop a b c  where
  -- generators, 8 in all.
  unit :: (a b c :> a b c :< a b c)
  counit :: (a b c :> a b c :< a b c) -> (a b c)
  -- rules


-- Circuit quick!
data C = Light | Switch | Battery deriving (Eq, Ord, Show)

newtype Apex a = Apex { runApex :: a } deriving (Eq, Ord, Show)

-- cospans should have types distinct in the magnitude of sets i x and o
newtype Cospan i x o = Cospan (i :> Apex x :< o)


mkCospan :: x -> Map.Map i x -> Map.Map o x -> Cospan i x o
mkCospan x i o = Cospan (i :> x :< o)

newtype VIs a = VIs { runVIs :: LCircSet }

-- monoidal product in cospan
mprod :: Cospan i x o -> Cospan i' x' o' -> Cospan i'' x'' o''
mprod a b = mkCospan
  where
    lgraph' = Set.disjointUnion 
    

blackbox (VIs a :> LGraph lg :< VIs a') = undefined


type L = L

type LCircSet c l = Set.Set (C, L)

type S = LCircSet -> Node

type T = LCircSet -> Node

type Graph = (LCircSet, S, T)

