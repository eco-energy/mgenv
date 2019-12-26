module TransmissionSpec where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import Test.Hspec
import Physics.Transmission


instance Arbitrary TransmissionSpec where
  arbitrary = arbitrary

-- If a TransmissionSpec is a semigroup it must oey the associativity law
-- here we use quickcheck to ~prove that it holds for TransmissionSpec

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigoupAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

main = do
  quickCheck (monoidAssoc :: TransmissionSpec -> TransmissionSpec -> TransmissionSpec -> Bool)

-- The averaging of all the elements mean that associativity doesn't hold
