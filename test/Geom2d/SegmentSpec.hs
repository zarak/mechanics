module Geom2d.SegmentSpec (spec) where

import Geom2d.Point
import Geom2d.Segment
import Geom2d.TParameter
import Test.Hspec
import Prelude hiding (length)

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
    it "returns another point when there is an intersection" $ do
      let self = Segment (Point 300 300) (Point 700 400)
          other = Segment (Point 700 400) (Point 300 500)
          expected = Point 451.49 400
      other `intersectionWith` self `shouldBe` pure expected
