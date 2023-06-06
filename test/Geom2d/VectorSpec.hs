module Geom2d.VectorSpec (spec) where

-- import Geom2d.Nums (areCloseEnough)
import Geom2d.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Invariant
import Test.QuickCheck

newtype VectorWrapper = VectorWrapper Vector deriving (Eq, Show)

instance Arbitrary VectorWrapper where
  arbitrary = do
    u <- arbitrary
    VectorWrapper . Vector u <$> arbitrary

commutativeWrapper :: VectorWrapper -> VectorWrapper -> Bool
commutativeWrapper (VectorWrapper v1) (VectorWrapper v2) = commutative (^+^) v1 v2

spec :: Spec
spec = do
  describe "^+^" $ do
    it "adds the corresponding components of two vectors" $ do
      let v = Vector 1 2
          u = Vector 4 6
          expected = Vector 5 8
      u ^+^ v `shouldBe` expected

  prop "is commutative" $ do
    commutativeWrapper
