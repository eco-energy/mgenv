{-# LANGUAGE TypeOperators, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, RankNTypes, GADTs, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFoldable, GeneralisedNewtypeDeriving, StandaloneDeriving, DerivingStrategies, TupleSections, PatternSynonyms #-}
module Physics.Symplectic where

import GHC.Generics hiding (R, D, L, Rep)
import Control.Newtype.Generics
import ConCat.Category
import ConCat.Misc
import ConCat.Free.VectorSpace
import ConCat.Free.LinearRow
import ConCat.Isomorphism
import ConCat.Additive
import Prelude hiding (id, (.), const, curry, uncurry)
import Prelude (Int)

import Data.Bifunctor
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck
import Data.Functor.Rep
import Data.Key
import Data.Pointed

import Physics.VectorSpaces

type Bilinear s c v i = (L s c (L s v i))



type Symplectic s c v i = Bilinear s c v i -> (v -> i)

symp :: (Bilinearity s c v i, Linear s ((V s v :-* V s i) s)) => Bilinear s c v i -> (c -> L s v i)
symp b = lapply b


type Scalar s = (IsScalar s, Num s)
type Linear s i = (Additive i
                  , HasV s i
                  , (Foldable (V s i))
                  , Pointed (V s i)
                  , Representable (V s i)
                  , Eq (Rep (V s i))
                  , Zip (V s i)
                  )

type Bilinearity s c v i = (Scalar s, Linear s c, Linear s v, Linear s i)


bilinearForm :: forall s c v i. (Bilinearity s c v i) => Symplectic s c v i
bilinearForm = undefined -- @(Symplectic s c v i)

point' :: forall f a. (Representable f) => a -> f a
point' = Data.Functor.Rep.tabulate . const



-- symplect :: forall s c v i.
--   (Bilinearity s c v i) => Symplectic s c v i -> (v :* i -> s)
-- symplect = lapply
  -- let
  -- vx :: (v -> v')
  -- vx = lapply (exl vi')
  -- ix :: (i -> i')
  -- ix = lapply (exr vi')
  -- vi'' = lapply vi
  -- in undefined
--class Symplectic s a b where
--  w :: L s a b -> L s a b ->  L s a d
  

prop_bilinearity :: Property
prop_bilinearity = forAll (arbitrary @((Rule Integer))) bilinear

newtype D a = D { unD :: a }
  deriving (Generic)
  deriving anyclass Newtype

newtype Rule a = Rule { unRule :: Binop a :* (a :* a :* a) :*  ((a :* a) -> D a) :* D a :* D a }
  deriving Generic
  deriving anyclass Newtype

type RuleF a = (Binop a, (a :* a :* a), ((a :* a) -> D a), D a, D a)

--pattern RuleF :: forall a. Rule a -> RuleF a
--pattern (RuleF (Rule ((((f, uvw), f'), dv), dw))) = RuleF (f, uvw, f', dv, dw)

ruleIso :: Iso (->) (Rule a) (RuleF a)
ruleIso = fw :<-> bw
  where
    fw (Rule ((((f, uvw), f'), dv), dw)) = (f, uvw, f', dv, dw) 
    bw (f, uvw, f', dv, dw) = Rule ((((f, uvw), f'), dv), dw)

params :: Rule a -> (a :* a :* a)
params = snd . fst . fst . fst . unRule

instance (Show a) => Show (Rule a) where
  show = ("Rule! Params:: " <>) . show . params

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Rule a) where
  arbitrary = (isoRev ruleIso) <$> c
    where
      c :: Gen (Binop a, (a :* a :* a), ((a :* a) -> D a), D a, D a)
      c = (,,,,)
          <$> arbitrary @(Binop a)
          <*> arbitrary @(a :* a :* a)
          <*> ((fmap (fmap D)) $ arbitrary @(a :* a -> a))
          <*> (fmap D $ arbitrary @a) 
          <*> (fmap D $ arbitrary @a)
  
bilinear :: forall a. (Num a, Eq a) => Rule a -> Bool
bilinear (Rule ((((b', ((u, v), w)), λb), D λu), D λv)) =
                      ((b (u + v, w)) == (b (u, w) + b (v, w)))
                      && (b (λu, v) == (unD $ λb (u, v)))
                      && (b (u, v + w) == (b (u, v) + b (u, w)))
                      && (b (u, λv) == (unD . λb $ (u, v)))
  where
    b :: (a, a) -> a
    b = uncurry b'
