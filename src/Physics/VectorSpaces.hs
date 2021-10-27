{-# LANGUAGE TypeOperators, GADTs, RankNTypes #-}
module Physics.VectorSpaces where

import Physics.Time
import ConCat.Isomorphism
import ConCat.Free.VectorSpace
import ConCat.Free.LinearRow
import ConCat.Free.Affine
import ConCat.Misc

type PowerIso p v i = p <-> v :< i

type EnergyIso t p e = t :* p <-> e


data v :< i = v :<: i






