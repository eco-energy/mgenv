module Semanticsv1 where

{--
newtype LCirc i o l v = LCirc { runLCirc :: (LGraph l v, Cospan i o v) }

instance (Monoid l, Monoid v) => Category (LCirc l v) where
  id = LCirc id
  (.) (LCirc LGraph g :# Cospan i o g'') (LCirc LGraph g' :# Cospan i' o' c' g''') = LCircZ

mkCirc :: l -> v -> LCirc l v
mkCirc l v = (mkLGraph l v) :# toCospan


class LCircuit x n y l' e n where
  i :: x -> n
  o :: y -> n
  l :: e -> l'
  s :: e -> n
  t :: e -> n

instance Category LCirc R (VI R) where
  (.) (LCirc LGraph g :# Cospan c) (LCirc LGraph g' :# Cospan c') = LCirc LGraph (g <> g) :# Cospan c''
    where
      c'' = composeCospan c  c'


  -- the id here is the zeroth node. We should have as id a perfectly conductive wire
  id = (mkLCirc . mkGraph) perfectlyConductiveWire

perfectlyConductiveWire :: Edge R (VI R)
perfectlyConductiveWire = Edge (Node mkVI (0,0) :# Node mkVI (0, 0)) :# 0

composeGraph (LGraph g) (LGraph g') =  g . g'

copair :: (e -> l) -> (e' -> l) -> (e :# e') -> l
copair i o = undefined

-- coproducts

coprod :: (e -> n) -> (e' -> n') -> (e :# e') -> (n :# n')
coprod f f' (e :# e') = (f e :# f' e')

sCoprod :: (e :# e') -> (n :# n')
sCoprod = coprod s s'
  where
    s = source
    s' = source

tCoprod :: (e :# e') -> (n :# n')
tCoprod = coprod t t'
  where
    t = target
    t' = target

--naturalMapFromCoproductToPushout :: (n :# n') -> ()

composeLCirc (LCirc (LGraph g :# Cospan k x n y)) (LCirc (LGraph g' :# Cospan k' x' n' y')) = Circ (LGraph addToLGraph :# composeCospan)

type LCircR = LCirc R (VI R)

type LCircZ = LCirc Z (VI R)


newtype Cospan k x n y = Cospan { runCospan :: (x `k` n, y `k` n) } deriving (Functor)

composeCospan :: Cospan k x n y -> Cospan k x n y -> Cospan k x n y
composeCospan (Cospan k x n y) (Cospan k' x' n' y') = Cospan (k . k') (x <> x') (n <> n') (y <> y')

tensorCospans :: [Cospan k x n y] -> Cospan k x n y
tensorCospans = undefined

data a :. b = a :. b

type Node' a b = Either a :. b Terminal

data Terminal a = Input a | Output a

type VICospan k = Cospan (:.) (Terminal VI R) (Terminal VI R) (Node' VI k)


circToCospan :: LCirc l v -> Terminal VI -> Output VI -> VICospan
circToCospan = undefined
--}
