{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.PolygonSpec where

import Geom2d.Point (Point (..))
import Geom2d.Polygon (centroid, containsPoint, mkPolygon, sides)
import Geom2d.Segment (Segment (..))
import Test.Hspec

spec :: Spec
spec = do
  let vertices = [Point 0 0, Point 30 0, Point 0 30]
      polygon = mkPolygon vertices
  describe "sides" $ do
    it "connects a list of vertices of a polygon with segments" $ do
      let expected =
            [ Segment (head vertices) (vertices !! 1),
              Segment (vertices !! 1) (vertices !! 2),
              Segment (vertices !! 2) (head vertices)
            ]
      sides <$> polygon `shouldBe` Just expected
  describe "centroid" $ do
    it "is the arithmetic mean of the position of all vertices" $ do
      let expected = Point 10 10
      centroid <$> polygon `shouldBe` Just expected
  describe "containsPoint" $ do
    context "when the polygon does not contain a point" $ do
      it "returns False" $ do
        let point = Point 15 20
        containsPoint point <$> polygon `shouldBe` Just False
    context "when the polygon does contain a point" $ do
      it "returns True" $ do
        let point = Point 15 10
        containsPoint point <$> polygon `shouldBe` Just True
    context "when the polygon contains a point lying on an edge" $ do
      it "returns True" $ do
        containsPoint (head vertices) <$> polygon `shouldBe` Just True
