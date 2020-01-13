module Common where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers



testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
  foldr (>>) (return ()) (map (uncurry it) tests)


(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (counterexample . ("Extra Info: " ++))
infixl 2 <?>
