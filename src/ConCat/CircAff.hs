{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module ConCat.CircAff where

import qualified Data.Set as Set
import qualified Data.Map as Map


import GHC.Generics (Generic)

--import Control.Arrow
--import qualified Control.Category as C
import Prelude hiding (id, (.))

import ConCat.Free.VectorSpace hiding ((^+^))

import ConCat.Category
import ConCat.Additive
import ConCat.Pair
import ConCat.Rep (HasRep(..))
import ConCat.Circuit
import ConCat.Free.Affine

import Data.Pointed
import Control.Monad.Representable.State

-- Context
-- Network Diagrams of various systems have analogies.
-- In each case, the system's state is described by variables that come in pairs,
-- with one variable in each pair playing the role of 'displacement' and the other playing the role of 'momentum'.
-- In engineering, the time derivatives of these variables are sometimes called 'flow' and 'effort'.
-- This pairing of variables can be understood using symplectic geometry. Thus any mathematical formulation of the diagrams of
-- these networks needs to take symplectic geometry as well as category theory into account.
--                 | displacement q    | flow q'    | momentum p    | effort p'
-- electronics     | charge            | current    | flux linkage  | voltage


type R = Double
type S = [R]
newtype Z' a = Z' (a, a, a) -- impedance as an RLC tuple

type Z = Z' R
type Voltage = Double
type Current = Double

deriving instance Pointed Z'

newtype VI a = VI { unVI :: Pair a } deriving (Show, Functor, Generic)

instance (Eq a) => Eq (VI a) where
  (==) (VI (a :# a')) (VI (b :# b')) = a == b && a' == b' 

instance (Ord a, Num a) => Ord (VI a) where
  (<=) a b = power a <= power b

deriving instance Pointed VI

-- use the HasRep instance to wrap the tuple
mkVI :: (a, a) -> VI a
mkVI = VI . abst

fst' :: Pair a -> a
fst' (a :# _) = a

snd' :: Pair a -> a
snd' (_ :# b) = b

-- Denotations and Calculations
voltage :: VI a -> a
voltage = fst' . unVI -- first would be cleaner. Make Pair an instance of bifunctor

current :: VI a -> a
current = snd' . unVI

power :: (Num a) => VI a -> a
power vi = voltage vi * current vi

-- Nodes
newtype Node v = Node { getNode :: v } deriving (Eq, Ord, Show, Functor, Generic)

mkNode :: v -> Node v
mkNode = Node
unNode :: Node v -> v
unNode (Node a) = a

deriving instance Pointed Node

nodeVoltage :: Node (VI a) -> a
nodeVoltage = getNode . (fmap voltage) 

nodeCurrent :: Node (VI a) -> a
nodeCurrent = getNode . (fmap current)

nodePower :: (Num a) => Node (VI a) -> a
nodePower = getNode . (fmap power)

zeroNode :: (Num a) => Node (VI a)
zeroNode = mkNode zeroV


newtype EdgePair a = EdgePair {nodePair :: Pair a} deriving (Show, Generic)

deriving instance Pointed EdgePair

instance (Eq a) => Eq (EdgePair a) where
  (==) (EdgePair (a :# a')) (EdgePair (b :# b')) = a == a' && b == b'

instance (Ord a) => Ord (EdgePair a) where
  (<=) (EdgePair (a :# a')) (EdgePair (b :# b')) = a <= a' && b <= b'

-- Edge Traversal
newtype Edge l v = Edge { getEdge :: (EdgePair (Node v), l) } deriving (Eq, Ord, Show, Generic)

-- same for order
--instance (Ord l, Ord v) => Ord (Edge l v)

deriving instance Pointed (Edge l)

type Source' l v = Edge l v -> Node v

type Target l v = Edge l v -> Node v

type VIEdge a = Edge (VI a) a

zeroEdge' :: (Num a) => VI a -> VI a -> VIEdge a
zeroEdge' from to = undefined -- (mkEdge (mkNode from) (mkNode to) 0

source :: Source' l v
source = fst' . nodePair . fst . getEdge

target :: Target l v
target = snd' . nodePair . fst . getEdge

label :: Edge l v -> l
label = snd . getEdge

-- an edge is a pair of nodes paired with a label.
mkEdge :: Node v -> Node v -> l -> Edge l v 
mkEdge n n' l = Edge (EdgePair (abst (n, n')), l)


zeroEdge :: (Pointed p, Num l, Num vi) => Edge (p l) (VI vi)
zeroEdge = mkEdge zeroNode zeroNode $ zeroV

oneEdge :: (Additive (f a1), Pointed f, Num a2, Num a1, Num (f a1)) => p -> Edge (f a1) (VI a2)
oneEdge l = mkEdge zeroNode zeroNode (zeroV ^+^ 1)

edgeVoltage :: (Num a) => Edge l (VI a) -> a
edgeVoltage e@(Edge (EdgePair (n :# n'), _)) = (nodeVoltage . source) e - (nodeVoltage . target) e

--ohmsLaw edge = impedance edge * current edge
edgeCurrent :: (Num a) => Edge a (VI a) -> a
edgeCurrent edge = (label edge) * (edgeVoltage edge)

resistance :: Edge R (VI a) -> R
resistance = label

impedance :: Edge Z (VI a) -> Z
impedance = label

data RLC a where
  -- explore type indexed list representation
  Resistor :: a -> RLC a
  Inductor :: a -> RLC a
  Capacitor :: a -> RLC a

data E = Res | Ind | Cap deriving (Eq, Ord, Show)

--type R = Double
--type S = (Double, Double, D)

--rlc :: (Num a) => a -> E -> RLC a
rlc p Res = Resistor p
rlc p Ind = Inductor p
rlc p Cap = Capacitor p

unRLC :: RLC a -> a
unRLC (Resistor a) = a
unRLC (Inductor a) = a
unRLC (Capacitor a) = a

--instance HasRep (RLC a) where

toRLC ps es = fmap rlc $ map (,) $ zip ps es

--instance (Num a) => HasV a (RLC a) where
--  toV = unRLC
--  unV = rlc
  

instance (Num a) => HasV a (VI a) where
  -- toV :: a -> V s a s
  toV = undefined
  unV = undefined
  -- unV :: V s a s -> a

instance (Num a) => Additive (VI a) where
  zero = zeroV
  (VI (v :# i)) ^+^ (VI (v' :# i')) = (VI ((v + v') :# (i + i')))


{------------------------------------------------------------------------------------------------------------------------

                              LGraphs and Their Cospans

-------------------------------------------------------------------------------------------------------------------------}


newtype NodeSet v = NodeSet (Set.Set (Node (VI v))) deriving (Eq, Ord, Show, Generic)

nodeSet = NodeSet

newtype EdgeSet l v = EdgeSet (Set.Set (Edge l v)) deriving (Eq, Ord, Show, Generic)

edgeSet = EdgeSet

type LabelMap l v = Map.Map (Edge l v) l


type VINode a = Node (VI a)

-- An LGraph is a product of N : ID -> a and E : (ID, ID) -> l

data LGraph l v = LGraph
  { nodes :: NodeSet v
  , edges :: EdgeSet l v
  } deriving (Show, Generic)

mkLGraph :: (Additive l, Num v) => NodeSet v -> EdgeSet l v -> LGraph l v
mkLGraph = LGraph


-- Basic Circuit Elements
type Resistor = LGraph Z (VI Z)

{--
unitR :: Node (VI a) -> (Node  (VI a)) State m Resistor
unitR = mkLGraph (source zeroEdge,  target zeroEdge)
  zeroEdge
--}
type Inductor = LGraph Z (VI Z)

type Capacitor = LGraph Z (VI Z)

type VoltageSource = LGraph Z (VI Z)

type CurrentSource = LGraph Z (VI Z)

-- rules for 'Components!' -> m inputs and n outputs
cospan :: LGraph () (VI Z) -> LGraph () (VI Z) -> LGraph Z (VI Z)
cospan i o = undefined

infix 0 :->
infix 1 :<-
  
data i :-> x =  i :-> x
data x :<- o = x :<- o

newtype Cospan i n o = Cospan (i :-> n :<- o)

idEdgeSet :: EdgeSet l v
idEdgeSet = EdgeSet Set.empty

toCospan :: LGraph l v -> (NodeSet v) -> (NodeSet v) -> Cospan (LGraph () v) (LGraph l v) (LGraph () v)
toCospan n i o = Cospan (LGraph {nodes=i, edges=idEdgeSet} :-> n :<- LGraph {nodes=o, edges=idEdgeSet}) 


-- these represent maps into a finite set.
-- inputs associate to the right and outputs associate to the left. So b is the finite set of lgraphs,
-- a is the 
--

{--

Fix a set L. Then by Proposition 2.2.2, Circ L is equivalent to a prop. Henceforth, by a
slight abuse of language, we use Circ L to denote this prop. To understand Circ L as a prop
first notice one very important fact: the object 1 equipped with the generating cospans
m : 2 → 1, i : 0 → 1, d : 1 → 2 and e : 1 → 0, thought of as L-circuits with no edges, is a
special commutative Frobenius monoid in Circ L . It is no coincidence that m, i, d, and e
subject to the laws of a special commutative Frobenius monoid are also the generators of
FinCospan.
The idea is that the labelled edges in an L-circuit correspond to elements of L, while
cospans m, i, d, and e let us put the labelled edges together in interesting ways. In the prop
framework a labelled edge in an L-circuit is thought of as a morphism ` : 1 → 1 which is
59labelled by some element of L. If the prop Circ L were generated only by morphisms ` like
this then the morphisms of Circ L would only look like parallel edges.
Thus the overall idea is that an L-circuit can be built from generating cospans and
labelled edges
--}

type CospanL l v = Cospan (LGraph () v) (LGraph l v) (LGraph () v)

type FinCospan v = Cospan (LGraph () v) (LGraph () v) (LGraph () v)

data a :-- b = a :-- b

newtype LCirc l v = LCirc ((LGraph l v),  (FinCospan v))

{--
conductiveEdge = LGraph {nodes=n [0, 1], edges=e ((0, 1)) 0}
  where
    n :: (Num b, Ord b) => [(VI b)] -> NodeSet b
    n a = NodeSet (Set.fromList (map mkNode a))
    e :: (Num b, Ord b, Ord l) => (Node (VI b), Node (VI b)) -> l -> EdgeSet l (Node (VI b))
    e (n, n') l = EdgeSet (Set.fromList [mkEdge (mkNode n) (mkNode n') l])

instance Category LCirc where
  id = conductiveEdge
  (.) = composeLCirc
--}

collapse :: LGraph l v -> NodeSet v
collapse (LGraph { nodes }) = nodes

--coprod :: CospanL l v -> CospanL l v -> CospanL l v
coprod :: (Num v, Ord v, Ord l) => Cospan (LGraph l v) (LGraph l v) (LGraph l v) -> Cospan (LGraph l v) (LGraph l v) (LGraph l v) -> Cospan (LGraph l v) (LGraph l v) (LGraph l v)
coprod (Cospan c@(LGraph { nodes=NodeSet i } :-> LGraph { nodes=NodeSet x, edges=EdgeSet e } :<- LGraph { nodes=NodeSet o } ))
  (Cospan c'@(LGraph { nodes=NodeSet i' } :-> LGraph { nodes=NodeSet x', edges= EdgeSet e' } :<- LGraph { nodes=NodeSet o' } )) = Cospan (LGraph {nodes= NodeSet (Set.union i i'), edges=idEdgeSet} :-> LGraph {nodes=(NodeSet (Set.union x x')), edges=(EdgeSet (Set.union e e'))} :<- LGraph {nodes=NodeSet (Set.union o o'), edges=idEdgeSet})


comp (Cospan c@(LGraph { nodes=i } :-> LGraph { nodes=NodeSet x, edges=EdgeSet e } :<- LGraph { nodes=NodeSet o } ))
  (Cospan c'@(LGraph { nodes=NodeSet i' } :-> LGraph { nodes=NodeSet x', edges= EdgeSet e' } :<- LGraph { nodes= o' } )) = Cospan (LGraph {nodes=i, edges=idEdgeSet} :-> LGraph {nodes=(NodeSet (pushout x x' i')), edges=(EdgeSet (Set.union e e'))} :<- LGraph {nodes=o', edges=idEdgeSet})
  where
    pushout :: Set.Set a -> Set.Set a -> Set.Set a -> Set.Set a
    pushout a b i = identify (Set.disjointUnion a b) i
      where
        identify :: Set.Set (Either a a) -> Set.Set a -> Set.Set a
        identify = undefined


inputs :: LCirc l v -> NodeSet v
inputs (LCirc (_, (Cospan (LGraph{ nodes=i} :-> _ :<- _)))) = i

outputs :: LCirc l v -> NodeSet v
outputs (LCirc (_, (Cospan (_ :-> _ :<- LGraph{ nodes=o})))) = o

terminals :: (Ord v, Num v) => LCirc l v -> NodeSet v
terminals (LCirc (_, (Cospan (LGraph {nodes=NodeSet i} :-> _ :<- LGraph{nodes=NodeSet o})))) = NodeSet (Set.union i o)

--composeLCirc :: (Ord v, Ord l, Num v, Num l) => LCirc l v -> LCirc l v -> LCirc l v
--composeLCirc = comp

{--(LCirc (lg@LGraph{ nodes = n0, edges = e0},
                     Cospan (i :-> n :<- o))) (LCirc (lg' @ LGraph{ nodes = n0', edges = e0'},
                                                      Cospan (i' :-> n' :<- o'))) = LCirc (lgraph'', cospan'')
  where
    lgraph'' = undefined  -- LGraph{nodes=(n0 +++ n0'), edges=(e0 +++ e0')}
    cospan'' = Cospan (f i :-> n'' :<- f' o')
    f = undefined
    f' = undefined
    n'' = undefined
--}

--unitLGraph = zeroEdge

{--


-- Two cospans from X to Y are isomorphic if there exists an isomorphism f i :: G -> G' such that
-- (Cospan x G y) = (Cospan x (f i G) y) = Cospan x G' y
-- basically if the edges labels are the same but the names of the nodes are different.


pushout :: Cospan i o l v -> Cospan i o l v -> Cospan i o l v
pushout (Cospan (x :-> n :<- y)) (Cospan (x' :-> n' :<- y')) = (Cospan (x :-> n'' :<- y'))
  where
    n'' = collapse disUnion 
    disUnion = (Set.disjointUnion n n') Set.\\ divisor
    divisor = Set.partition (\(x, y)-> x == y)
    collapse = undefined
    

data Null

newtype Cospan i o l v = Cospan (LGraph Null i :-> LGraph l v :<- LGraph Null o)

{--
FinCospan is a symmetric monoidal category where
    - the tensor product is the disjoint union, denoted by +,
    - the unit object is the empty set.
    - The braiding morphism B : m + n → n + m is given by the cospan (m + n :→ m + n :← n + m)
        where the image of m under :-> is the same as the last m of the apex,
        the image of n under :-> is the first n of the apex,
        the image of m under :<- is the first m of the apex,
        and the image of n under :<- is the last n of the apex.

--}



cospan x i o = Cospan (i :-> x :<- o)

{--
class Category k where
  type Ok k :: Type -> Constraint
  type Ok k = Yes1
  id  :: Ok k a => a `k` a
  infixr 9 .
  (.) :: forall b c a. Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)
--}

instance Category (LCirc l v) where
  --id :: Ok k a => a `k` a
  id = undefined
  -- (.) :: forall b c a. Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)
  a . b = composeCospan a b

instance AssociativePCat (LCirc l v) where
  lassocP = undefined
  rassocP = undefined

instance MonoidalPCat (LCirc l v) where
  -- tensor product...
  (***) = undefined

instance BraidedPCat (LCirc l v) where
  swapP = undefined


i :: Cospan i o l v -> i
o :: Cospan i o l v -> o
i (Cospan (i :-> x :<- _)) = i
o (_ :-> x :<- o) = o

-- cospan composition
composeCospan :: Cospan i o l v -> Cospan i o l v -> Cospan i o l v
composeCospan = undefined -- cospan unionOverOAndi' i o'
  where
    unionOveri' = ((Set.\\ Set.filter (x /= x')) . Set.disjointUnion x x')

-- write tests for composition and tensoring.
tensorCospan :: Cospan i o l v -> Cospan i o l v -> Cospan i o l v
tensorCospan (Cospan c) (Cospan c') = undefined



-- A prop is a symmetric monoidal category
-- LCirc has lgraphs as objects and cospans as morphisms.
--class (Category a) => Prop a where
  


-- The semantic function is blackboxing




type P = VI R



-- can we learn an id and a morphism for a compositional category?
-- perhaps symmetric monoidal ones. Can we learn a tensor product and a direct sum through attention?


-- semantics of LCirc Morphism Vocabulary
series = composeCospan
parallel = tensorCospan




type EVI = EdgeSet R (VI R)

type NVI = NodeSet (VI R)





type LCircOb = LGraph EVI NVI

newtype LCircMorphism l v = LCircMorphism { unLCM :: (LGraph l v, LGraph l v, LGraph l v) }

data LCirc e v i o where
  Object :: (e, v, i, o) -> LCirc e v i o
  Moprhism ::  LCirc e v i o -> LCirc e v i o -> LCirc e v i o
  
  


mkLGraph' :: (Applicative f) => f l -> f v -> LGraph l v
mkLGraph' = undefined

-- pairs are two elements of the same type,
-- so 
degenerateLGraph :: NodeSet v -> (LGraph Bool v)
degenerateLGraph v = LGraph ((False :# ), False)
  where
    e' = Set.empty
    v' = v

genNode = mkVI

--genGraph n gen = Set.map (genNode gen)

--zeroedGraph n = genGraph n (\_ -> (0, 0))


instance Category LGraph where
--  id = LGraph (() :# ())
  id = undefined
  a . b = undefined
--  a . b = undefined 



{--------------------------------------------------------------------------------------------------
                              Operational Semantics of Cospans of LGraphs. 

--------------------------------------------------------------------------------------------------}


type Input v = NodeSet v

type Output v = NodeSet v

inputs :: (Cospan i o l v) -> Input v
inputs (Cospan (LGraph g, LGraph i, LGraph o)) = i

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


-- Check this definition!
tensorCospan' :: Cospan i o l v -> Cospan i o l v -> Cospan i o l v
tensorCospan' (Cospan (LGraph g, NodeSet i, NodeSet o)) (Cospan (LGraph g', NodeSet i', NodeSet o')) = Cospan (g'', i'', o'')
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



{--------------------------------------------------------------------------------------------------------------

                              Electrical Composition Rules for LGraphs

---------------------------------------------------------------------------------------------------------------}

-- A boundary is specified by a terminal set

type TerminalSet v = Set.Set (Node v)

type NonTerminalSet v = Set.Set (Node v)

terminalNodes :: NodeSet v -> TerminalSet v
terminalNodes = undefined

nonTerminalNodes :: NodeSet v -> TerminalSet v -> NonTerminalSet v
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



kcl :: (Num b, Num b, Ord b) => NonTerminalEdges l v -> Source l v -> Target l v -> b
kcl nt s t = (sum s' - sum t') == 0
  where
    s' = Set.map (\e-> (current . getNode) (s e)) nt
    t' = Set.map (\e-> (current . getNode) (t e)) nt


-- The net inflow and outflow over a given set of terminal nodes is as follows
boundaryCurrent :: (Num b, Num b, Ord b) => TerminalEdges l v -> Source l v -> Target l v -> b
boundaryCurrent ts s t = sum t' - sum s'
  where
    s' = Set.map (\e-> (current . getNode) (s e)) ts
    t' = Set.map (\e-> (current . getNode) (t e)) ts




extendedPowerFunctional :: (Num a, Ord a) => EdgeSet l (VI a) -> a
extendedPowerFunctional rs es = ((1/2) * (sum $ Set.map p' es))
  where
    p' e = ((edgeVoltage e)**2) / label e

powerFunctional :: (Num b, Num b) => TerminalSet v -> LabelMap l v -> b
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



-- semantic function
-- blackbox = (finCospan :*: (\labelSet -> affineLagrangian . h . finCospan . labelSet))

-----------------------------------------------------------------------


type VINode = Node P

type Label = R


{--
-- LGraph NEEDS to be data as it has to be an instance of a category
class (HasV e, HasV n, Additive n) => LGraph' e n where
  -- e is the edge label
  -- n is the node type
--  type Edge = Edge e n
--  type Node = Node n
  s' :: Edge e n -> n
  t' :: Node e n -> n
--}



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




--instance SymplecticVectorSpace Circ Additive VI



--instance C Circ where
--  (.) :: CircL l -> (FinCospan :*: F l -> AffLagRel k)


data a :% b = a :% b deriving (Functor)

-- (:%) a b = blackbox


{--
Blackbox is a transformation over props.
- We need base CircL prop constructors - RLC - transistor - diode
- a definition for composition over CircL.
- a Symplectic Vectorspace over k defined by an affine lagrangian relation.
-- a label Field over a set of potential and current measurements
-- a Field of composition ordinarily impedence
--}

{--
randCirc :: MonadSample ()
randCirc = do
  cspec <- ask
  (\c-> runCircuit c i o where (i, o) = terminals c) . mkCircuit =<< cspec
--}
--}
