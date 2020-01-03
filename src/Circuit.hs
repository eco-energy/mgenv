{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Circuit where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Algebra.Graph.Labelled as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as GM
import qualified Algebra.Graph.Class as GC

import GHC.Generics (Generic)

import Scan (Pair(..), mkPair, BT, Scan(..))

--import Control.Arrow
--import qualified Control.Category as C
import Prelude hiding (id, (.))

import ConCat.Free.VectorSpace
import ConCat.Free.Affine

import ConCat.AD
import ConCat.Category



-- Context
-- Network Diagrams of various systems have analogies.
-- In each case, the system's state is described by variables that come in pairs,
-- with one variable in each pair playing the role of 'displacement' and the other playing the role of 'momentum'.
-- In engineering, the time derivatives of these variables are sometimes called 'flow' and 'effort'.
-- This pairing of variables can be understood using symplectic geometry. Thus any mathematical formulation of the diagrams of
-- these networks needs to take symplectic geometry as well as category theory into account.

--                 | displacement q    | flow q'    | momentum p    | effort p'
-- electronics     | charge            | current    | flux linkage  | voltage

-- Circuits build from components that respond linearly to an applied voltage.
-- Does not include diodes or nonlinear resistors: linear resistors, capacitors and inductors
-- Extension of this framework to props allows dealing with more general circuits (non-linear resistors, V and I sources, transistors etc).

-- a linear resistor dissipates power, turning useful energy into heat at a rate determined by the voltage across the resistor
-- a circuit made out of such a resistor always acts to MINIMIZE the power dissipated this way
-- 'principle of minimum power' is why symplectic geometry is important.

-- Open Circuits for Resistances


type R = Double
type S = [R]
type Z = (R, R, R) -- impedance
type Voltage = Double
type Current = Double


newtype VI a = VI { unVI :: Pair a } deriving (Functor, Eq, Ord, Show)


mkVI :: (a, a) -> VI a
mkVI = VI . mkPair

type P = VI R

newtype Node v = Node { getNode :: v } deriving (Eq, Ord, Show, Functor)

newtype NodeSet v = NodeSet (Set.Set (Node v))

instance (Semigroup v) => Semigroup (Node v) where
  (<>) (Node v) (Node v') = v <> v'

instance (Monoid v) => Monoid (Node v) where
  mempty = mempty

newtype Edge l v = Edge { getEdge :: Pair (Pair (Node v), l) } deriving (Eq, Ord, Show, Functor)

instance Semigroup (Edge l v) where
  

type EdgeSet l v = Set.Set (Edge l v)

type Source l v = Edge l v -> Node v

type Target l v = Edge l v -> Node v

type LabelMap l v = Map.Map (Edge l v) R

type Input v = NodeSet v

type Output v = (v -> NodeSet v)


newtype LGraph l v = LGraph { runLGraph :: (G.Graph (Edge l v) (Node v)) } deriving (Show, Generic)


mkLGraph :: (Functor f) => f l -> f v -> LGraph l v
mkLGraph e v = undefined



instance Semigroup (LGraph l v) where
  (<>) (LGraph g) (LGraph g') = LGraph (G. g g')


newtype Cospan i o l v = Cospan { runCospan :: (LGraph l v, Input i, Output o)} 

nullSet = Set.empty



type LCircC a b = (HasV a b) => Cospan (VI a) (VI a) b b


-- i and o are degenerate l-graphs where the set of edges l is empty.
attachInput :: Input v -> LGraph l v -> Cospan i o l v
attachInput = mkCospan 

attachOutput :: Output v -> LGraph l v -> Cospan i o l v
attachOutput = undefined

inputs :: Cospan i o l v -> Input v
inputs (Cospan (LGraph g, NodeSet i, NodeSet o)) = i

outputs :: Cospan i o l v -> Output v
outputs (Cospan (LGraph g, NodeSet i, NodeSet o)) = o

unitCospan :: LGraph l v -> Cospan i o l v
unitCospan = flip . flip Cospan

-- N +y N'
data lgraph :+- cospan = lgraph :+-- cospan deriving (Generic)



seqCospan :: Cospan i o l v -> Cospan o o' l v -> Cospan fi fo' l v
seqCospan (Cospan c) (Cospan c') = Cospan i o'' l' v'
  where
    l' :: (l -> l)
    l' = undefined
    v' = undefined



tensorCospan :: Cospan i o l v -> Cospan i o l v -> Cospan i o l v
tensorCospan (Cospan (LGraph g, NodeSet i, NodeSet o)) (Cospan (LGraph g', NodeSet i', NodeSet o')) = Cospan (g'', i'', o'')
  where
    e'' = Set.union G.edgeSet g G.edgeSet g'
    g'' = Set.union G.vertexSet g G.vertexSet g'
    n = G.vertexSet g
    n' = G.vertexSet g'
    e = G.edgeSet g
    e' = G.edgeSet g'
    l = (\(Edge ((_ :# l))) -> l)
    l' = l
    i'' = \n -> Set.union i n i' n -- how do you tensor morphisms?
    o'' = \n -> Set.union o n o'n


input :: VI a -> Input (VI a)
input vi = Node vi

output :: VI a -> Output (VI a)
output vi = Node vi

-- Ohms Law, Kirchoff's Laws and the Principle of Minimum Power

voltage :: (Num a, Fractional a) => VI a -> a
voltage = first . unVI

current :: (Num a, Fractional a) => VI a -> a
current = second . unVI

nodeVoltage :: Node (VI a) -> a
nodeVoltage = fmap voltage

nodeCurrent :: Node (VI a) -> a
nodeCurrent = fmap current

-- TODO: Use first and second
source :: Source a b
source e = first . first . getEdge

target :: Target a b
target e = second . first . getEdge

label :: Edge l v -> l
label = second . getEdge

resistance :: Edge R (VI a) -> R
resistance = label

impedance :: Edge Z (VI a) -> Z
impedance = label

data RLC a where
  Resistor :: a -> RLC a
  Inductor :: a -> RLC a
  Capacitor :: a -> RLC a


ohmsLaw :: Edge l v -> Voltage
ohmsLaw (Edge a VI vi) = impedance edge * current vi
ohmsLaw (Edge a VI(vi)) = resistance edge * current vi

edgeVoltage :: (Num v, Fractional v) => Edge l v -> v
edgeVoltage e = (nodeVoltage . source e) - (nodeVoltage . t e)


class AffLegRel k where
  a :: k

-- SEMANTIC FUNCTION
--blackbox :: LCirc l v -> AffLegRel k
--blackbox = undefined



--class Cospan a where
--  toCospan :: a -> Cospan a
--  fromCospan :: Cospan a -> a
--  cospanSum     :: a :++ b
--  cospanProduct :: a +** b
--  cospanUnit    :: Cospan a
--  cospanTensorProduct :: Tensor (Cospan a) (Cospan a)
--




--instance Category (LGraph) where
  -- the identity is the id LGraph
--  id = LGraph . id
--  (.) = undefined
  -- the morphisms are the

{--
newtype LCirc i o l v = LCirc { runLCirc :: (LGraph l v, Cospan i o v) }

instance (Monoid l, Monoid v) => Category (LCirc l v) where
  id = LCirc id
  (.) (LCirc LGraph g :# Cospan i o g'') (LCirc LGraph g' :# Cospan i' o' c' g''') = LCircZ

mkCirc :: l -> v -> LCirc l v
mkCirc l v = (mkLGraph l v) :# toCospan

{--
class LCircuit x n y l' e n where
  i :: x -> n
  o :: y -> n
  l :: e -> l'
  s :: e -> n
  t :: e -> n
--}

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


--newtype Additive a = Additive

--newtype Duplicative a = Duplicative

--type AdditiveCirc = Additive Circ

--type DuplicativeCirl = Duplicative Cric


-- class CommutativeFrobeniusStructure x y where
--   ux ::
--   nx ::
--   dx ::
--   ex ::

-- class SpecialCommutativeFrobeniusMonoid a where
-- instance SpecialCommutativeFrobeniusMonoid AdditiveCirc where
-- instance SpecialCommutativeFrobeniusMonoid DuplicativeCirc where

-- voltage should be replaced by phi, a potential over R^n, in this case the electrical potential


-- A boundary is specified by a terminal set
type TerminalSet v = Set.Set (Node v)

type NonTerminalSet v = Set.Set (Node v)

terminalNodes :: NodeSet v -> TerminalSet v
terminalNodes = undefined

nonTerminalNodes :: (Ord a) => NodeSet v -> TerminalSet v -> NonTerminalSet v
nonTerminalNodes ns ts = ns `Set.difference` ts


-- A boundary potential is a function in R^dn, where dn is the set of terminals of the circuit.
-- Given our open circuit, we are free to choose the boundary potential variables
boundaryPotential :: TerminalSet v -> Voltage
boundaryPotential = undefined


-- Kirchoff's current law holds if for all non-terminal nodes
-- The total current flowing into or out of any non-terminal nodes is zero
type NonTerminalEdges l v = Set.Set (Edge l v)
type TerminalEdges l v = Set.Set (Edge l v)

terminalEdges :: EdgeSet l v -> TerminalSet v -> TerminalEdges l v
terminalEdges = undefined

nonTerminalEdges :: EdgeSet l v -> TerminalSet v -> NonTerminalEdges l v
nonTerminalEdges e v = e `Set.difference` (terminalEdges e v)



kcl :: (Num b, Fractional b, Ord b) => NonTerminalEdges a -> Source a -> Target a -> b
kcl nt s t = (sum s' - sum t') == 0
  where
    s' = Set.map (\e-> (current . getNode) (s e)) nt
    t' = Set.map (\e-> (current . getNode) (t e)) nt


-- The net inflow and outflow over a given set of terminal nodes is as follows
boundaryCurrent :: (Num b, Fractional b, Ord b) => TerminalEdges l v -> Source v -> Target v -> v
boundaryCurrent ts s t = sum t' - sum s'
  where
    s' = Set.map (\e-> (current . getNode) (s e)) ts
    t' = Set.map (\e-> (current . getNode) (t e)) ts




extendedPowerFunctional :: (Fractional a, Ord a) => EdgeSet l (VI a) -> a
extendedPowerFunctional rs es = ((1/2) * (sum $ Set.map p' es))
  where
    p' e = ((edgeVoltage e)**2) / label e

powerFunctional :: (Num b, Fractional b) => TerminalSet a -> LabelMap (VI b) -> b
powerFunctional terminalN rs = (Set.map (extendedPowerFunctional rs) (terminalEdges terminalN))

-- We care a bunch about potential



-- Category Theoretic things
-- Decorated Cospans
-- Cospan Categories
-- Hypergraph Categories
-- Decorated Cospan Categories
-- Open Circuits and their semantics
-- Open Circuits
-- Dirichlet Cospan Semantics
-- Lagrangian Subspaces
-- Lagrangian Relations
-- Symplectification
-- Legrangian Cospan Semantics
-- Black Box Functor
-- Decoratated Corelations
-- Corelation Categories
-- Decorated Corelation Categories
-- Constructing the Black Box Functor
-- The Semantics of Ideal Wires
-- Lagrangian Corelations
-- Black Box Functor
-- The trinity of minimization, symplectification and Kirchoff's laws
-- Dirichlet Corelations
-- Composition through Power Minimization




-- Building Circuits


-- The Circuit Type is a labelled graph over VI nodes.
-- The circuit category has as id:
-- id = zeroV (== ideal wire circuit)
-- (.) a b = overlay a b
-- There are three kinds of nodes, Input, Output and Terminal
-- L-Circuit is a cospan of finite sets
-- Circ -G-> FinCospan -H-> FinCorel

-- G maps objects in Circ: Graph e v -> Cospan FinSet v
-- G maps the overlay morphism to
-- H takes the objects in Graph v to


{--
class Cospan x y n where
  i' :: x -> n
  o' :: y -> n
--}

-- semantic function
-- blackbox = (finCospan :*: (\labelSet -> affineLagrangian . h . finCospan . labelSet))

-----------------------------------------------------------------------


type VINode = Node P

type Label = R

data LG = LG
  { nodes :: NodeSet P
  , edges :: EdgeSet ((VINode :# VINode) :# Label)
  }

class LGraph' e n where
  s' :: e -> n
  t' :: e -> n

{--
class (Cospan (Node n Node n Node n) LGraph e n) => LCirc' e n where
  inputs :: NodeSet n
  outputs :: NodeSet n
  terminals :: TerminalSet n
--}

mkNode :: Node (VI a)
mkNode = Node mkVI (0, 0)


class FinCospan a where
  finCospan :: a

class FinRel k where
-- k is an extraspecial commutative Frobenius Monoid
-- k is parameterized in two ways - the duplicative frobenius structure and the additive frobenius structure.
-- The duplicative structure allows us to work with electric potential, because an ideal wire circuit splitting duplicates
-- the potential because of kvl.
-- The additive structure allows us to work with currents, because kcl.
  finRel :: k

-- class (Field l, HasLAction l v) => Circ l v where
  -- l are the edges and x the vi nodes.
  -- an L-action of a set L on an object x in a category C is a function a :: l -> homset (x, x)
  -- given two L-actions a :: l -> hom (x, x) and b :: l -> hom (y, y), a morphism of L-actions is a morphism
  --          f :: x -> y in C such that f . a $ l = (b l) . f for all l in L.
  --
  -- co-product category of two props, one for special commutative Frobenius monoids and the second for L-actions
  -- has morphisms l :: L -> map (UnOp l) L


-- k is a lear relation R subset k^2n that it imposes between potentials and currents at its inputs and outputs
--  circ :: G.Graph l v

--class (LCirc e n, FinRel k, Functor b) => BlackBox b c k where
--  circuitToBehaviour :: c  -> k

--runBB :: (Circ c, FinRel k, Functor b, Scan f) => f k -> c -> BlackBox b c k -> k
--runBB i c = circuitToBehaviour c

-- Definition of the unique morphism of props. from CircL


-- This is how to deal with categories:
-- A category C is defined in haskell by providing the type (structure) of morphisms in C, instead of explicitly stating its objects and morphisms.


{--
It is possible to encode a category in haskell, but it doesn’t look exactly the same as laying out its definition on paper in a mathematical setting. In particular:

  - A category C is defined in haskell by providing the type (structure) of morphisms in C, instead of explicitly stating its objects and morphisms.
  - Objects of all categories defined in haskell are types expressible by the haskell type system. In the whole world of mathematics, objects can be much broader.
  - Typechecking implementations of id and ∘ must be provided for all Category instances, which guides us towards law-abiding implementations.

--}


{--
newtype Circ f a = Circ { runCirc :: G.Graph f VI } deriving (Eq, Ord, Show)


instance SymplecticVectorSpace Circ Additive VI


newtype Prop a f = Prop { unProp :: a }


instance C Circ where
  (.) :: CircL l -> (FinCospan :*: F l -> AffLagRel k)

data a :[ b = undefined
--a (:%) b = blackbox


{--
Blackbox is a transformation over props.
- We need base CircL prop constructors - RLC - transistor - diode
- a definition for composition over CircL.
- a Symplectic Vectorspace over k defined by an affine lagrangian relation.
-- a label Field over a set of potential and current measurements
-- a Field of composition ordinarily impedence


--}




  where
    affineLagrangian = undefined
--}
--}
