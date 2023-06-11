{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use null" #-}
module Geom2d.SegmentSpec (spec) where

import Geom2d.Line (Line (..))
import Geom2d.Point
import Geom2d.Segment
import Geom2d.TParameter
import Geom2d.Vector (Vector (..), isParallelTo, isPerpendicularTo)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (length)

newtype SegmentWrapper = SegmentWrapper Segment deriving (Eq, Show)

instance Arbitrary SegmentWrapper where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    y1 <- arbitrary
    y2 <- arbitrary
    let s = Segment (Point x1 x2) (Point y1 y2)
    if length s > 0
      then pure $ SegmentWrapper s
      else pure $ SegmentWrapper (Segment (Point 0 0) (Point 1 1))

spec :: Spec
spec = do
  let start = Point 400 0
      end = Point 0 400
      segment = Segment start end

  describe "length" $ do
    it "computes the length of the segment" $ do
      let expected = 400 * sqrt 2
      length segment `shouldBe` expected

  describe "pointAt" $ do
    it "gives a validation error if t is not in the range [0, 1]" $ do
      let t = mkTParameter 1.2
      t `shouldBe` Nothing
    it "creates a TParameter type if t is in the range [0, 1]" $ do
      let t = case mkTParameter 0.2 of
            Just a -> a
            _ -> error "Invalid 't' parameter is not in range"
      unTParameter t `shouldSatisfy` (\t' -> 0 <= t' && t' <= 1)
    it "gets the midpoint of a segment" $ do
      let expected = Point 200 200
      middle segment `shouldBe` expected
    it "gets another midpoint of a segment" $ do
      let s1 = Point 40 10
          s2 = Point 60 10
          seg1 = Segment s1 s2
          expected = Point 50 10
      middle seg1 `shouldBe` expected
  describe "closestPoint" $ do
    context "when the outside point is unaligned with the segment" $ do
      it "is the start of the segment" $ do
        let p = Point 500 20
            expected = segment.start
        closestPoint p segment `shouldBe` expected
      it "is the end of the segment" $ do
        let p = Point 20 500
            expected = segment.end
        closestPoint p segment `shouldBe` expected
    context "when the outside point is aligned with the segment" $ do
      it "is the middle of the segment" $ do
        let p = Point 250 250
            expected = Point 200 200
        closestPoint p segment `shouldBe` expected
  describe "intersectionWith" $ do
    it "does not return a point when there is no intersection" $ do
      let other = Segment (Point 200 0) (Point 0 200)
      segment `intersectionWith` other `shouldBe` Nothing
    it "does return a point when there is an intersection" $ do
      let other = Segment (Point 0 0) (Point 400 400)
          expected = Point 200 200
      segment `intersectionWith` other `shouldBe` pure expected
  describe "bisector" $ do
    it "should be diagonal for a diagonal segment" $ do
      let seg = Segment (Point 0 0) (Point 10 10)
          actual = direction $ bisector seg
          expected = Vector -1 1
      -- Note that I chose the expected value to specifically be parallel to
      -- the result
      actual `shouldSatisfy` isParallelTo expected
    it "computes bisector of a horizontal segment" $ do
      let seg = Segment (Point 0 0) (Point 2 0)
      let expectedBisector = Line (Point 1 0) (Vector 0 1)
      direction (bisector seg) `shouldBe` direction expectedBisector
    prop "is perpendicular to the segment" $ do
      let propIsPerpendicularToBisec (SegmentWrapper seg) =
            directionVector seg `isPerpendicularTo` direction (bisector seg)
      propIsPerpendicularToBisec
