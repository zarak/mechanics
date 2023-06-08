module Geom2d.AffineTransfSpec where

import Geom2d.AffineTransf
  ( AffineTransform (..),
    applyToPoint,
    applyToPolygon,
    applyToRect,
    applyToSegment,
    defaultTransf,
  )
import Geom2d.Point (Point (..))
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Rect qualified as Rect (toPolygon)
import Geom2d.Segment
import Geom2d.Size
import Test.Hspec

spec :: Spec
spec = do
  let point = Point 2 3
      segment = Segment (Point 1 1) (Point 2 2)
      polygon = mkPolygon [Point 0 0, Point 3 0, Point 0 9]
      rect = Rect (Point 0 0) (Size 10 10)
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
  describe "applyToSegment" $ do
    it "scales a segment" $ do
      let expected = Segment (Point 2 5) (Point 4 10)
      applyToSegment scale segment `shouldBe` expected
  describe "applyToPolygon" $ do
    it "scales a polygon" $ do
      let expected = mkPolygon [Point 0 0, Point 6 0, Point 0 45]
      (polygon >>= applyToPolygon scale) `shouldBe` expected
  describe "applyToRect" $ do
    it "scales a rectangle" $ do
      let expected = Rect.toPolygon $ Rect (Point 0 0) (Size 20 50)
      applyToRect scale rect `shouldBe` expected
