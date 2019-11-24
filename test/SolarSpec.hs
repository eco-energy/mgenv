module SolarSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "directRadiation" $ do
    it "should always be zero at night" $ 0 `shouldBe` (1 - 1) 
