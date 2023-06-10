module Geom2d.CirclesSpec (spec) where

import Geom2d.Circle
import Geom2d.Circles
import Geom2d.Point (Point (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "circleFromPoints" $ do
    it "creates a circle given three points" $ do
      let a = Point -10 0
          b = Point 0 10
          c = Point 10 0
          actual = mkCircleFromPoints a b c
          expected = Circle mempty 10
      actual `shouldBe` Just expected
    it "creates another circle given three points" $ do
      let a = Point 300 300
          b = Point 700 400
          c = Point 300 500
          actual = mkCircleFromPoints a b c
          expected = Circle (Point 487.5 400) 212.5
      actual `shouldBe` Just expected
