module GeometrySpec where

import Physics.Units (EuclideanC, Meters, Theta)
import Geometry.EMST (minSpanTreeEdges, verticesToTree, pathToEdges, branches, positiveGridPoints, btwn0n360)
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec
import qualified Data.Set as Set
import Data.Tree

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => NE.NonEmpty a -> Bool
hasNoDups = (loop Set.empty) . NE.toList
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                  | otherwise
                    = False

pts :: Gen (NE.NonEmpty EuclideanC)
pts = suchThat arbitrary (\l -> length l > 2)

idPts :: Gen (NE.NonEmpty (Int, EuclideanC))
idPts = do
  ps <- pts
  return $ NE.zip (NE.fromList [0..]) ps 

angPts :: Gen [(Meters, Theta)]
angPts = suchThat arbitrary (\l -> length l > 2)

thetas :: Gen (Theta)
thetas = arbitrary


propCompleteTree = do
  forAll idPts (\ps -> (length . verticesToTree) ps == (length ps) <?> (show $ verticesToTree ps) ++ " should be " ++ (show (length ps)))

prop_oneLessEdgeThanNodesParsed = do
  forAll idPts (\ps -> length (minSpanTreeEdges ps) == ((length ps) - 1) <?> (show $ length (minSpanTreeEdges ps)) ++ " should be " ++ (show $ length ps - 1))

prop_NoDuplicateEdges = do
  forAll idPts (\ps -> (length $ Set.fromList (minSpanTreeEdges ps)) == (length $ minSpanTreeEdges ps))

prop_AllPositive = do
  forAll angPts (\ps ->  (\(xs, ys) -> (foldl min 0 xs >= 0) && (foldl min 0 ys >= 0)) (unzip (positiveGridPoints ps)) )

{--
PROVE LATER
prop_consistentDistances = do
  let
    pwDistPolar (ps1, ps2) = sqrt (
      (r1**2) + (r2**2) - (2*r1*r2*(atan2 (toRad t2) (toRad t1) )))
      where
        (r1, t1) = ps1
        (r2, t2) = ps2
        toRad = (*(pi/180)) . btwn0n360
    pwDistCart (ps1, ps2) = (x1 - x2)**2 + (y1 - y2)**2
      where
        (x1, y1) = ps1
        (x2, y2) = ps2
    pairs ps = zip ps (tail ps) 

  forAll angPts (\ps->
                   all (\(ps1, ps2)->
                          pwDistPolar ps1 == pwDistCart ps2)
                   (zip (pairs ps) (pairs $ positiveGridPoints ps))
                   <?>
                   ("should be \n" ++
                    (show $ map pwDistPolar (pairs ps)) ++
                    " is\n  " ++
                    (show $ map pwDistCart (pairs $ positiveGridPoints ps))
                   )
                )
--}
prop_angNorm = do
  forAll thetas (\p -> (\t-> t >= 0 && t <= 360) (btwn0n360 p) )

spec = do
  describe "Geometry tests" $ do
    it "paths should be nice" $ do
      let
        a = [1, 2, 3, 4, 5, 6, 7] :: [Int]
        b = [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6,7)]
      pathToEdges a `shouldBe` b
    it "tree branches should be nice" $ do
      let
        t = unfoldTree (\x -> if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])) (1 :: Int)
      branches t `shouldBe` [[1, 2, 4], [1, 2, 5], [1, 3, 6], [1, 3, 7]]
    it "(len tree) == len v" $ do
      property propCompleteTree
    it "(len v) - 1 == len e holds true for edges too" $ do
      property prop_oneLessEdgeThanNodesParsed
    it "should have no duplicates" $ do
      property prop_NoDuplicateEdges
    it "all grid points should be positive" $ do
      property prop_AllPositive
    it "checks whether angle normalization is solid" $ do
      property prop_angNorm
    --it "pairwise distances of all points in descending order should be the same" $ do
    --  property prop_consistentDistances
