{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Geometry.EMST (minSpanTreeEdges, verticesToTree, pathToEdges, branches, positiveGridPoints, btwn0n360) where

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
    g = withEdgeDistances squaredEuclideanDist . toPlaneGraph (Proxy :: Proxy MSTW)
      . delaunayTriangulation $ pts
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


positiveGridPoints :: [(Meters, Theta)] -> [EuclideanC]
positiveGridPoints xs = shift cs
  where
    shift :: [(Meters, Meters)] -> [(Meters, Meters)]
    shift ps = zip (map scaleX ps) (map scaleY ps)
    scaleX :: (Meters, Meters) -> Meters
    scaleX = (+rightwards) . fst
    scaleY :: (Meters, Meters) -> Meters
    scaleY = (+upwards) . snd
    rightwards = abs (foldl min 0 $ map fst cs)
    upwards = abs (foldl min 0 $ map snd cs)
    cs = map toCartesian xs
    toCartesian :: (Meters, Theta) -> (Meters, Meters)
    toCartesian (r, th) = (r*sin nth, r * cos nth)
      where
        nth = (toRadians . btwn0n360) th
        toRadians = (*(pi/180))


btwn0n360 :: Theta -> Theta
btwn0n360 n
  | n <= 360 && n >= 0 = n
  | n > 360 = n - (360*((fromIntegral . floor) ((n/360))))
  | n < 0 && n > (-360) = n + 360
  | n < (-360) = n + (360*((fromIntegral . ceiling) ((n/360))))
  | otherwise = n
