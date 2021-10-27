{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Geometry.EMST (minSpanTreeEdges, verticesToTree, pathToEdges, branches, positiveGridPoints) where

import Algorithms.Geometry.DelaunayTriangulation.Naive
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Graph.MST
import Control.Lens
import Data.Ext
import Data.Geometry
import qualified Data.List.NonEmpty as NE
import Data.PlaneGraph
import Data.Proxy
import Data.Tree

import Physics.Units 
import qualified Data.Set as Set

-- | Computes the euclidiean min spanning tree by computing the delaunay triangulation
-- and then extracting the EMST

euclideanMST :: (Ord r, Fractional r) => NE.NonEmpty (Point 2 r :+ p) -> Tree (Point 2 r :+ p)
euclideanMST pts = (\v -> g^.locationOf v :+ g^.dataOf v) <$> t
  where
    g = withEdgeDistances squaredEuclideanDist
        . toPlaneGraph (Proxy :: Proxy MSTW)
        . delaunayTriangulation
        $ pts
    t = mst $ g^.graph


data MSTW

minSpanTreeEdges :: (Ord a) => NE.NonEmpty (a, EuclideanC) -> [(a, a)]
minSpanTreeEdges = NE.toList . edgesFromTree . verticesToTree

verticesToTree :: (Ord r, Fractional r) => NE.NonEmpty (a, (r, r)) -> Tree (Point 2 r :+ a)
verticesToTree locs = euclideanMST $ NE.map (\(a, loc) -> ((:+ a) . uncurry point2) loc) locs

edgesFromTree :: (Ord a) => Tree (Point 2 Meters :+ a) -> NE.NonEmpty (a, a)
edgesFromTree t = NE.map extNodeId l
  where
    l = NE.fromList $ Set.toList $ Set.fromList $ concatMap pathToEdges $ branches t
    extNodeId x = ((_extra . fst) x, (_extra . snd) x)

pathToEdges :: [a] -> [(a, a)]
pathToEdges p = zip p (tail p)

branches :: Tree a -> [[a]]
branches (Node x []) = [[x]]
branches (Node x ts) = map (x:) (concatMap branches ts)


positiveGridPoints :: [(Meters, BearingDeg)] -> [EuclideanC]
positiveGridPoints = shift . map toCartesian

shift :: [(Meters, Meters)] -> [(Meters, Meters)]
shift ps = zip (map (scaleX (rightwards ps)) ps) (map (scaleY (upwards ps)) ps)

rightwards :: (Functor f, Foldable f) => f (EuclideanC) -> Meters
rightwards = abs . foldl min 0 . fmap fst

upwards :: (Functor f, Foldable f) => f (EuclideanC) -> Meters
upwards = abs . foldl min 0 . fmap snd

scaleX :: Meters -> (Meters, Meters) -> Meters
scaleX right = (+right) . fst

scaleY :: Meters -> (Meters, Meters) -> Meters
scaleY up = (+up) . snd

toCartesian :: (Meters, BearingDeg) -> (Meters, Meters)
toCartesian (r, th) = (r*sin (toRadians th), r * cos (toRadians th))
