module GridSpec where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec
import Grid

spec = do
  describe "check grid generation" $ do
    it "nothing" $ do
      1 `shouldBe` 1
