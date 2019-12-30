{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Circuit where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Algebra.Graph.Labelled as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Algebra.Graph.Class as GC
import Scan (Pair(..), mkPair, BT, Scan(..))
import Control.Arrow

import Data.VectorSpace

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

newtype Node v = Node { getNode :: v } deriving (Eq, Ord, Show, Functor)

type NodeSet v = Set.Set (Node v)

instance (Semigroup v) => Semigroup (Node v) where
  (<>) (Node v) (Node v') = v <> v' 

instance (Monoid v) => Monoid Node v where
  mempty = mempty



data Edge v l = Edge { getEdge :: Pair (Pair (Node v), l) } deriving (Eq, Ord, Show, Functor)

type EdgeSet v l = Set.Set (Edge v l)

type Source v l = Edge v l -> Node v

type Target v l = Edge v l -> Node v

type LabelMap v l = Map.Map (Edge v l) R

type Input v = (v -> Node v)

type Output v = (v -> Node v) 

input :: Input (VI a) (VI a)
input vi = Node vi

output :: Output (VI a) (VI a)
output vi = Node vi

-- Ohms Law, Kirchoff's Laws and the Principle of Minimum Power

voltage :: (Num a, Fractional a) => VI a -> a
voltage = first . unVI

current :: (Num a, Fractional a) => VI a -> a
current = second . unVI

nodeVoltage :: (Node VI a) -> a
nodeVoltage = fmap voltage

nodeCurrent :: (Node VI a) -> a
nodeCurrent = fmap current

-- TODO: Use first and second
source :: Source a
source e = first . first . getEdge

target :: Target a
target e = second . first . getEdge

label :: Edge l v -> l
label = second . getEdge

resistance :: Edge R (VI a) -> R
resistance = label

impedance :: Edge Z (VI a) -> Z
impedance = label

ohmsLaw :: Edge l v -> (Edge l v -> R) -> (Edge l v -> Current) -> Voltage
ohmsLaw edge r' i' = (r' edge * i' edge) 

edgeVoltage :: (Num v, Fractional v) => Edge l v -> v
edgeVoltage e = (nodeVoltage . source e) - (nodeVoltage . t e)

newtype LGraph l v = LGraph { runLGraph :: Graph (Edge l v) (Node v) } deriving (Eq, Ord, Functor, Show)

newtype Circ l v = Circ { runCirc :: LGraph l v :# Cospan (a -> v)  a v a}

copair :: l :*: l' -> (e -> l) -> (e -> l')
copair 


composeViaPushout (Circ (LGraph g :# Cospan _ x n y)) (Circ (LGraph g' :# Cospan _ x' n' y')) = Circ (LGraph addToLGraph :# composeCospan)

instance Category CircZ where
  (.) = undefined
                                          

--compose :: Circ l v -> Circ l v -> 

type CircR = Circ R (VI R)

type CircZ = Circ Z (VI R)


newtype Cospan k x y z = Cospan { runCospan :: (x `k` y, z `k` y) } deriving (Functor)

type VICospan = Cospan (:.) (VI R) (VI R) (VI R)

circToCoSpan :: CircR -> Inputs VI -> Outputs VI -> VICospan


mkCirc :: Circ
mkCirc = Circ

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

type NonTerminalSet a = Set.Set (Node v)

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
boundaryCurrent :: (Num b, Fractional b, Ord b) => TerminalEdges a -> Source a -> Target a -> b
boundaryCurrent ts s t = sum t' - sum s'
  where
    s' = Set.map (\e-> (current . getNode) (s e)) ts
    t' = Set.map (\e-> (current . getNode) (t e)) ts


extendedPowerFunctional :: (Fractional a, Ord a) => EdgeSet l (VI a) -> Power
extendedPowerFunctional rs es = ((1/2) * (sum $ Set.map p' es))
  where
    p' e = ((edgeVoltage e)**2) / label e

powerFunctional :: (Num b, Fractional b) => TerminalSet a -> LabelMap (VI b) -> b
powerFunctional terminalN rs = (Set.map (extendedPowerFunctional rs) (terminalEdges terminalN))



                                   
-- Category Theoretic things
-- Decorated CoSpans
-- CoSpan Categories
-- Hypergraph Categories
-- Decorated CoSpan Categories
-- Open Circuits and their semantics
-- Open Circuits
-- Dirichlet CoSpan Semantics
-- Lagrangian Subspaces
-- Lagrangian Relations
-- Symplectification
-- Legrangian CoSpan Semantics
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


k :: Node VI R -> Node VI R -> VI R
k x y

class CoSpan x y n where
  i :: x -> n
  o :: y -> n



-----------------------------------------------------------------------
type P = VI R

type VINode = Node P

type Label = R

data LG = LG
  { nodes :: NodeSet P
  , edges :: EdgeSet ((VINode :# VINode) :# Label)
  }

class LGraph e n where
  s :: e -> n
  t :: e -> n


class (CoSpan (Node n Node n Node n) LGraph e n) => LCirc e n where
  inputs :: NodeSet n
  outputs :: NodeSet n
  terminals :: TerminalSet n


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

class (Field l, HasLAction l v) => Circ l v where
  -- l are the edges and x the vi nodes.
  -- an L-action of a set L on an object x in a category C is a function a :: l -> homset (x, x)
  -- given two L-actions a :: l -> hom (x, x) and b :: l -> hom (y, y), a morphism of L-actions is a morphism
  --          f :: x -> y in C such that f . a $ l = (b l) . f for all l in L.
  -- 
  -- co-product category of two props, one for special commutative Frobenius monoids and the second for L-actions
  -- has morphisms l :: L -> map (UnOp l) L
  

-- k is a lear relation R subset k^2n that it imposes between potentials and currents at its inputs and outputs
  circ :: G.Graph l v
  
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

class C k where
  type Ok k :: Type -> Constraint
  type Ok k = Yes1
  id :: Ok k a => a `k` a
  (.) :: forall b c a. Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

class Field r where
  f :: r

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



blackbox = (finCospan :*: (\labelSet -> affineLagrangian . h . finCospan . labelSet))
  where
    affineLagrangian = undefined
--}
