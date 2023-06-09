module Graphics.PrimitivesSpec where

import Geom2d.Point
import Geom2d.Rect
import Geom2d.Segment
import Graphic.Svg.Primitives qualified as Primitives
import Test.Hspec

spec :: Spec
spec = do
  let lineTemplate = "<line x1=\"{{x1}}\" y1=\"{{y1}}\" x2=\"{{x2}}\" y2=\"{{y2}}\" {{attrs}}/>"
  describe "segment" $ do
    it "should render an SVG segment" $ do
      let segment = Segment (Point 2 3) (Point 4 5)
          expected = "<line x1=\"2.0\" y1=\"3.0\" x2=\"4.0\" y2=\"5.0\" />"
      Primitives.segment segment [] lineTemplate `shouldBe` expected
