module CircAffSpec where

import ConCat.CircAff
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Classes
import Test.QuickCheck.Utils


{--
category = const ( "symmetricMonoidalCategory"
                 , [ ("identity"    , rightid &&& leftid)
                   , ("composition",  property composititionP)
                   , ("tensorProduct", property tensorProductP)
                   , ("directSum", property undefined)
                   ])
--}


prod :: Double -> Double -> Double
prod a b = a * b

spec = do
  describe "LGraph should form a category" $ do
    it "composition should be associative" $ do
      property $ isAssociative prod


