module GridSpec where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec
import Grid
import Physics.Units (haversine, reverseHaversine, location, bearing)

spec = do
  describe "calculations for grid formation" $ do
    it "local to global coordinate frames" $ do
      let
        p = location 60.0000 60.0000
        p' = location 60.00001 60.00001
        b = bearing p p'
        d = haversine p p'
        r = reverseHaversine p d b
        d_cond = abs (haversine p r - haversine p p')
        b_cond = abs (bearing p r - bearing p p')
      d_cond `shouldSatisfy` (\x -> x <= (1.0))
      b_cond `shouldSatisfy` (\x -> x <= (1.0))
