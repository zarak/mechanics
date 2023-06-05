module Geom2d.PointSpec (spec) where

import Geom2d.Nums (areCloseEnough)
import Geom2d.Point
import Test.Hspec

spec :: Spec
spec = do
  describe "distanceBetween" $ do
    it "computes the distance between two points" $ do
      let p = Point 1 2
          q = Point 4 6
          expected = 5
      distanceBetween p q `shouldSatisfy` areCloseEnough 1e-10 expected
