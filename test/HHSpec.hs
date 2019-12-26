module HHSpec where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec

import HH

import qualified Physics.PV ()

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>


-- prop_oneLessEdgeThanNodesParsed = do
--  forAll idPts (\ps -> length (minSpanTreeEdges ps) == ((length ps) - 1) <?> (show $ length (minSpanTreeEdges ps)) ++ " should be " ++ (show $ length ps - 1))


genSpec :: Gen HH
genSpec = undefined

prop_generation_scanl_all_positive = do
  forAll genSpec (\a -> undefined)
  
prop_consumption_scanl_all_positive = undefined

prop_storage_bounded_by_max_min = undefined
prop_storage_next_state_proportional_to_charge_flux = undefined

prop_transmission_input_fills_storage = undefined
prop_transmission_output_drains_storage = undefined

prop_generation_fills_storage = undefined
prop_consumption_drains_storage = undefined


spec = do
  describe "HH tests" $ do
    it "generation " $ do
      property prop_generation_scanl_all_positive
      property prop_generation_fills_storage
    it "consumption" $ do
      property prop_consumption_scanl_all_positive
      property prop_consumption_drains_storage
    it "storage" $ do
      property prop_storage_bounded_by_max_min
      property prop_storage_next_state_proportional_to_charge_flux
    it "transmission" $ do
      property prop_transmission_input_fills_storage
      property prop_transmission_output_drains_storage
