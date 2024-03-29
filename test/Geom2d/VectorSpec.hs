module Geom2d.VectorSpec (spec) where

import Geom2d.Nums (isCloseToZero)
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

spec :: Spec
spec = do
  let u = Vector 1 2
      v = Vector 4 6

  describe "^+^" $ do
    it "adds the corresponding components of two vectors" $ do
      let expected = Vector 5 8
      u ^+^ v `shouldBe` expected
    prop "is commutative" $ do
      let commutativeWrapper (VectorWrapper v1) (VectorWrapper v2) = commutative (^+^) v1 v2
      commutativeWrapper
  describe "^-^" $ do
    it "subtracts the corresponding components of two vectors" $ do
      let expected = Vector (-3) (-4)
      u ^-^ v `shouldBe` expected
  describe "dot" $ do
    it "computes the dot product between two vectors" $ do
      let expected = 16
      dot u v `shouldBe` expected
    prop "is commutative" $ do
      let commutativeWrapper (VectorWrapper v1) (VectorWrapper v2) = commutative dot v1 v2
      commutativeWrapper
  describe "cross" $ do
    it "computes the cross product between two vectors" $ do
      let expected = (-2)
      cross u v `shouldBe` expected
  describe "isParallelTo" $ do
    it "checks whether two vectors are parallel" $ do
      u `shouldNotSatisfy` isParallelTo v
    prop "is always true for a vector with itself" $ do
      let propParallelToSelf (VectorWrapper vec) = vec `isParallelTo` vec
      propParallelToSelf
  describe "isPerpendicularTo" $ do
    let vec = Vector (-2) 1
    it "checks whether two vectors are perpendicular" $ do
      u `shouldSatisfy` isPerpendicularTo vec
    it "is true for vectors with a dot product of zero" $ do
      vec `dot` u `shouldSatisfy` isCloseToZero
    it "checks whether two vectors are perpendicular" $ do
      u `shouldNotSatisfy` isPerpendicularTo v
