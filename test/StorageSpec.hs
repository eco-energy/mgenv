module StorageSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Physics.Storage


spec :: Spec
spec = do
  describe "test battery invariant properties" $ do
    it "doesn't matter" $ 1 `shouldBe` 1
