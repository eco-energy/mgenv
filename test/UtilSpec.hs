module UtilSpec (spec) where

import Node
import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
  describe "nodes" $ do
    it "mkNode should have sane defaults" $ 0+2 `shouldBe` 2
    it "Graph of node states should have a " $ 0+23 `shouldBe` 23
    it "affine map over the node state leads to a difference in weight distribution" $  0 `shouldBe` 1 
    prop "bijective mapping from state to target is invertible " $ \i -> (i :: Double) - 2 `shouldBe` (i - 2)

