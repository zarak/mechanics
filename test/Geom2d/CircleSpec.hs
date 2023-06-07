module Geom2d.CircleSpec (spec) where

import Geom2d.Circle
import Geom2d.Point (Point (..))
import Geom2d.Polygon (mkPolygon)
import Test.Hspec

spec :: Spec
spec = do
  let circle1 = Circle (Point 0 0) 1
      circle2 = Circle (Point 0 0) 2
  describe "area" $ do
    it "computes the area of a circle of radius 1" $ do
      let expected = pi
      area circle1 `shouldBe` expected
    it "computes the area of a circle of radius 2" $ do
      let expected = 4 * pi
      area circle2 `shouldBe` expected
  describe "area" $ do
    it "computes the circumference of a circle of radius 1" $ do
      let expected = 2 * pi
      circumference circle1 `shouldBe` expected
    it "computes the circumference of a circle of radius 2" $ do
      let expected = 4 * pi
      circumference circle2 `shouldBe` expected
  describe "toPolygon" $ do
    it "gives a polygon approximation to a given circle" $ do
      let expected = mkPolygon [Point 1 0, Point 0 1, Point (-1) 0, Point 0 (-1)]
      toPolygon circle1 4 `shouldBe` expected
