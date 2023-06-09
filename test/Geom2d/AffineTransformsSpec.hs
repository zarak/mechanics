module Geom2d.AffineTransformsSpec where

import Geom2d.AffineTransf
  ( applyToPoint,
  )
import Geom2d.AffineTransforms
import Geom2d.Point (Point (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "mkRotation" $ do
    it "rotates about the origin" $ do
      let p = Point 15 15
          rotOrigin = mkRotationOrigin (pi / 4)
      applyToPoint rotOrigin p `shouldBe` Point 1.7763568394002505e-15 21.213203435596427
    it "rotates about the the coordinates (10, 10)" $ do
      let p = Point 15 15
          rotOrigin = mkRotation (pi / 4) (Point 10 10)
      applyToPoint rotOrigin p `shouldBe` Point 10.000000000000002 17.071067811865476
