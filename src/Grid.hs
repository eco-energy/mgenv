{-# LANGUAGE MultiParamTypeClasses
, GADTs
, RankNTypes
, TypeOperators
, ConstraintKinds
, FlexibleContexts
, TypeFamilies
, DeriveGeneric
, PolyKinds
, FlexibleInstances
, TypeSynonymInstances
, TypeApplications
, ScopedTypeVariables
, DeriveAnyClass
, GeneralisedNewtypeDeriving
, DerivingStrategies
, DerivingVia
, DeriveFoldable
, DeriveFunctor
, DeriveTraversable
, StandaloneDeriving
, QuantifiedConstraints
, UndecidableInstances
#-}


module Grid where

import Prelude hiding (id, (.), curry, uncurry, zip, zipWith)
import ConCat.Misc hiding (C)
import qualified ConCat.Misc as CM
import Data.Complex
import qualified ConCat.Rep as CRep
import ConCat.Isomorphism
import ConCat.Synchronous
-- import ConCat.Complex
import ConCat.Continuation
import ConCat.Chain
import ConCat.Choice
import ConCat.Regress
import GHC.Generics ((:*:), (:.:), Generic, Generic1, Rep, Rep1, Rec1)
import ConCat.Free.Affine
import ConCat.Free.LinearRow
import ConCat.Free.VectorSpace
import ConCat.RAD
import ConCat.Category
import ConCat.Deep
import ConCat.Additive
import ConCat.Scan
--import ConCat.RegressChoice

--import ConCat.SMT
import Data.Monoid
import Data.Bifunctor
import Data.Functor.Rep
import Data.Key hiding (index)
import Data.Pointed

import qualified Algebra.Graph.Labelled as G

import Control.Newtype.Generics

deriving instance Pointed (L (Complex s) a)

instance HasV R (Complex R)


newtype Z s a b = Z { unZ :: (L s (Complex a) (Complex b)) }
  deriving (Generic)
  -- deriving newtype (Pointed)
  deriving anyclass (Newtype)

type Z'' a b = Z (Complex R) a b

--instance Pointed (V (Complex s) a)

 -- deriving newtype (Pointed)

instance CRep.HasRep (Z s a b) where
  type Rep (Z s a b) = (L s (Complex a) (Complex b))
  repr (Z lin) = lin
  abst lin = Z lin

zeroZ :: (RealFloat s, Pointed (V s (Complex a)), Pointed (V s (Complex b))) => Z s a b
zeroZ = Z zeroLM
  
--instance HasRep (S s a b)
deriving newtype instance (HasV s (CRep.Rep (Z s a b))) => HasV s (Z s a b)

type Z' a b = Z Double a b

newtype S s a b = S { unS :: Affine s (Complex a) (Complex b) }
  deriving (Generic)
  deriving anyclass (Newtype)

instance CRep.HasRep (S s a b) where
  type Rep (S s a b) = (Affine s (Complex a) (Complex b))
  repr (S aff) = aff
  abst aff = S aff
  
--instance HasRep (S s a b)
deriving newtype instance (HasV s (CRep.Rep (S s a b))) => HasV s (S s a b)

instance Category (Z s) where
  type Ok (Z s) = (Rs' s)
  id = pack id
  (.) = inNew2 (.)


toAffine :: (Rs s a, Rs s b) => Z s a b -> S s a b
toAffine = S . linearA . unZ

instance Category (S s) where
  type Ok (S s) = (Rs' s)
  id = pack id
  (.) = inNew2 (.)


instance (Rs s a, Rs s b) => Additive (S s a b) where
  zero = S zero
  (S aff) ^+^ (S aff') = S $ aff +. aff'
    where
      (+.) :: Binop (Affine s (Complex a) (Complex b))
      (+.) = (ConCat.Additive.^+^)
  {-# INLINE zero #-}
  {-# INLINE (^+^) #-}
  

applyS :: (OkGrid s a b) => S s a b -> (Complex a -> Complex b)
applyS = applyA . unS

applyZ :: (OkGrid s a b) => Z s a b -> (Complex a -> Complex b)
applyZ = lapply . unZ

type S' a b = S Double a b

newtype C s a b = C { unC :: Z s a b :+ S s a b }
  deriving (Generic)
  deriving anyclass (Newtype)

--deriving instance (Generic s, Generic1 a) => Generic1 (C s a)

instance CRep.HasRep (C s a b) where
  type Rep (C s a b) = (Z s a b) :+ (S s a b)
  repr (C (Left lin)) = Left lin
  repr (C (Right aff)) = Right aff
  abst (Left lin) = C (Left lin)
  abst (Right aff) = C (Right aff)
deriving newtype instance (HasV s (CRep.Rep (C s a b))) => HasV s (C s a b)

applyC :: (OkGrid s a b) => C s a b -> (Complex a -> Complex b)
applyC = either applyZ applyS . unC


instance Category (C s) where
  type (Ok (C s)) = Rs' s 
  id = C (Right id)
  (C (Left lin)) . (C (Left lin')) = C . Left $ lin . lin'
  (C (Left aff)) . (C (Right aff')) = C . Right $ (toAffine aff) . aff'
  (C (Right aff)) . (C (Left aff')) = C . Right $ aff . (toAffine aff')
  (C (Right aff)) . (C (Right aff')) = C . Right $ aff . aff'


--deriving instance Generic1 (G.Graph l)
newtype Gr s a b = Gr (G.Graph (Z s a b) (C s a b))
  deriving (Generic)


type V' s a = ( HasV s (Complex a)
              , Foldable (V s (Complex a))
              , Pointed (V s (Complex a))
              , Zip (V s (Complex a))
              , Representable (V s (Complex a))
              , Eq (Data.Functor.Rep.Rep (V s (Complex a))))

 
type VI s a b = (a :*: b) s

type Rs s a = (Additive a, RealFloat s, V' s a, RealFloat a, HasV s (Complex a))

class (Rs s a) => Rs' s a


type OkGrid s a b = (Rs s a, Rs s b)

d :: (Num s) => (C s a b -> s) -> Unop (C s a b)
d f = gradR f

type D s = s



-- instance Category (C s) where
--   type Ok (C s) = (Rs' s)
--   id = pack (Left idL)
--   C (Left b) . (C (Left a)) = C applyZ a . applyZ b
--instance 

--mkC :: (Ok2 (L s) a b) => (a -> b) -> C s a b
--mkC f = f <~ id

data Grid s a b where
  IdG :: Grid s a a
  Storage :: S s a b -> Grid s (C s a b) (C s a b)
  Generation :: Z s a b -> Grid s (C s a b) (C s a b)
  Consumption :: Z s a b -> Grid s (C s a b) (C s a b)
  Transmission :: Z s a b -> Grid s (C s a b) (C s a b)

-- runGrid :: forall s a b. (Rs s a, Rs s b) => Grid s (C s a b) (C s c d) -> C s a b
-- runGrid IdG = id @(C s)
-- runGrid (Storage (s :: (S s a b))) = C (Right s)
-- runGrid (Generation g) = C . Left $ g
-- runGrid (Consumption c) = C . Left $ c
-- runGrid (Transmission t) = C . Left $ t


--instance LScan (Gr s a)

idGrid :: Grid s a a
idGrid = IdG 

-- compG :: forall s a b c. (OkGrid s a b) => Grid s a b -> Grid s b c -> Grid s a c 
-- compG IdG IdG = IdG
-- compG IdG a = a
-- compG a IdG = a
-- compG (Storage (s :: S s a b)) (Storage (s' :: S s b c)) = Storage c
--   where
--     c :: S s a c
--     c = s . s'

-- instance Category (Grid s) where
--   type (Ok (Grid s)) = Rs' s
--   id = idGrid
--   (.) = flip compG


{--
instance (OkCirc s a b) => Category (C s) where
  type (Ok (C s)) = OkCirc
  id = id
  --(.) = (.)
  (C (Left lin)) . (C (Left lin')) = C . Left $ lin . lin'
  (C (Right aff)) . (C (Right aff')) = C . Right $ aff . aff'
--}




-- gridReward :: (OkGrid s a b) => (Gr s a b -> s) -> (Gr s a b -> s) -> (Gr s a b -> s) -> Gr s a b -> Sum s
-- gridReward demand supply storage gr = foldl (<>) (Sum 0) [ ((demand) gr )
--                                                          , ((supply) gr )
--                                                          , ((storage) gr)
--                                                          ]
--   where
--     timeDeriv f = gradR $ (\i-> index f i)

