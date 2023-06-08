{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Geom2d.AffineTransfSpec where

import Geom2d.AffineTransf
  ( AffineTransform (..),
    affThen,
    applyToPoint,
    applyToPolygon,
    applyToRect,
    applyToSegment,
    defaultTransf,
    inverse,
  )
import Geom2d.Point (Point (..))
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Rect qualified as Rect (toPolygon)
import Geom2d.Segment
import Geom2d.Size
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary)

newtype AffineTransformWrapper = AffineTransformWrapper AffineTransform deriving (Eq, Show)

-- Need to fix some values to ensure transform is invertible
instance Arbitrary AffineTransformWrapper where
  arbitrary = do
    -- sx <- arbitrary
    -- sy <- arbitrary
    tx <- arbitrary
    ty <- arbitrary
    -- shx <- arbitrary
    shy <- arbitrary
    pure $ AffineTransformWrapper (AffineTransform 1 1 tx ty 2 shy)

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
  describe "affThen" $ do
    it "concatenates scale and then translate" $ do
      let expected = defaultTransf {sx = 2, sy = 5, tx = 10, ty = 15}
      affThen scale trans `shouldBe` expected
    it "concatenates translate and then scale" $ do
      let expected = defaultTransf {sx = 2, sy = 5, tx = 20, ty = 75}
      affThen trans scale `shouldBe` expected
  describe "inverse" $ do
    it "computes the inverse of an affine transformation" $ do
      let transf = AffineTransform 1 2 3 4 5 6
          expected = defaultTransf
      transf `affThen` inverse transf `shouldBe` expected
    prop "of a transform composed with itself yields an identity transform" $ do
      let propInverseIdentity (AffineTransformWrapper affT) =
            affThen affT (inverse affT) == defaultTransf
      propInverseIdentity
