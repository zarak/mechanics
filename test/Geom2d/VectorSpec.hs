module Geom2d.VectorSpec (spec) where

-- import Geom2d.Nums (areCloseEnough)
import Geom2d.Vector
import Test.Hspec

spec :: Spec
spec = do
  describe "^+^" $ do
    it "adds the corresponding components of two vectors" $ do
      let v = Vector 1 2
          u = Vector 4 6
          expected = Vector 5 8
      u ^+^ v `shouldBe` expected
