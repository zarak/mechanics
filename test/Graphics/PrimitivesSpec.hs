module Graphics.PrimitivesSpec where

import Geom2d.Circle
import Geom2d.Point
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Segment
import Geom2d.Size
import Graphic.Svg.Primitives qualified as Primitives
import Test.Hspec

spec :: Spec
spec = do
  let lineTemplate = "<line x1=\"{{x1}}\" y1=\"{{y1}}\" x2=\"{{x2}}\" y2=\"{{y2}}\" {{attrs}}/>"
      rectTemplate = "<rect x=\"{{x}}\" y=\"{{y}}\" width=\"{{width}}\" height=\"{{height}}\" {{attrs}}/>"
      circTemplate = "<circle cx=\"{{cx}}\" cy=\"{{cy}}\" r=\"{{r}}\" {{attrs}}/>"
      polTemplate = "<polygon points=\"{{points}}\" {{attrs}}/>"
  describe "segment" $ do
    it "should render an SVG segment" $ do
      let segment = Segment (Point 2 3) (Point 4 5)
          expected = "<line x1=\"2.0\" y1=\"3.0\" x2=\"4.0\" y2=\"5.0\" />"
      Primitives.segment segment [] lineTemplate `shouldBe` expected
  describe "rectangle" $ do
    it "should render an SVG rectangle" $ do
      let rect = Rect (Point 2 3) (Size 4 5)
          expected = "<rect x=\"2.0\" y=\"3.0\" width=\"4.0\" height=\"5.0\" />"
      Primitives.rectangle rect [] rectTemplate `shouldBe` expected
  describe "circle" $ do
    it "should render an SVG circle" $ do
      let circle = Circle (Point 1 2) 5
          expected = "<circle cx=\"1.0\" cy=\"2.0\" r=\"5.0\" />"
      Primitives.circle circle [] circTemplate `shouldBe` expected

  describe "polygon" $ do
    it "should render an SVG polygon" $ do
      let actual = do
            p <- mkPolygon [Point 2 3, Point 4 5, Point 6 7]
            pure $ Primitives.polygon p [] polTemplate
          expected = "<polygon points=\"2.0,3.0 4.0,5.0 6.0,7.0\" />"
      actual `shouldBe` Just expected
