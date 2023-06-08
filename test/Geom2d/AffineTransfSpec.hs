module Geom2d.AffineTransfSpec where

import Geom2d.AffineTransf (AffineTransform (..), applyToPoint, defaultTransf)
import Geom2d.Point (Point (..))
import Test.Hspec

spec :: Spec
spec = do
  let point = Point 2 3
      scale = defaultTransf {sx = 2, sy = 5}
      trans = defaultTransf {sx = 1, sy = 1, tx = 10, ty = 15}
      shear = defaultTransf {sx = 1, sy = 1, shx = 3, shy = 4}

  describe "applyToPoint" $ do
    it "scales a point" $ do
      let expected = Point 4 15
      scale `applyToPoint` point `shouldBe` expected
    it "translates a point" $ do
      let expected = Point 12 18
      trans `applyToPoint` point `shouldBe` expected
    it "shears a point" $ do
      let expected = Point 11 11
      shear `applyToPoint` point `shouldBe` expected
