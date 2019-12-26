module HHSpec where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec

import Physics.PV
import Physics.Units (R, Watts)

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

instance Arbitrary Mount where
  arbitrary = (arbitrary :: Gen Mount)

instance Arbitrary ModuleType where
  arbitrary = (arbitrary :: Gen ModuleType)

instance Arbitrary PVSpec where
  arbitrary = PVSpec
              <$> (arbitrary :: Gen R)
              <*> (arbitrary :: Gen R)
              <*> (arbitrary :: Gen R)
              <*> (arbitrary :: Gen Watts)
              <*> (arbitrary :: Gen Mount)
              <*> ((arbitrary :: Gen ModuleType))


prop_modTemp_directly_proportional_to_irradiance_and_ambient_temp = do
  forAll undefined undefined
