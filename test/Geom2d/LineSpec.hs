module Geom2d.LineSpec (spec) where

import Geom2d.Line
import Geom2d.Point (Point (..))
import Geom2d.Vector (Vector (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "intersectionWith" $ do
    it "returns Nothing if the lines are parallel" $ do
      let l1 = Line (Point 0 0) (Vector 1 1)
          l2 = Line (Point 10 10) (Vector 1 1)
      l1 `intersectionWith` l2 `shouldBe` Nothing
    context "when the lines are perpendicular" $ do
      it "returns the point of intersection of nonparallel lines" $ do
        let l1 = Line (Point 50 0) (Vector 0 1)
            l2 = Line (Point 0 30) (Vector 1 0)
            expected = Point 50 30
        l1 `intersectionWith` l2 `shouldBe` pure expected
      it "yields the same result when negating one direction" $ do
        let l1 = Line (Point 50 0) (Vector 0 -1)
            l2 = Line (Point 0 30) (Vector 1 0)
            expected = Point 50 30
        l1 `intersectionWith` l2 `shouldBe` pure expected
      it "yields a point corresponding to the x-component" $ do
        let l1 = Line (Point 20 0) (Vector 0 -1)
            l2 = Line (Point 0 30) (Vector 1 0)
            expected = Point 20 30
        l1 `intersectionWith` l2 `shouldBe` pure expected
    context "when the lines are at a 45 degree angle" $ do
      it "intersects at the origin" $ do
        let l1 = Line (Point 1 0) (Vector 1 0)
            l2 = Line (Point 1 1) (Vector 1 1)
            expected = Point 0 0
        l1 `intersectionWith` l2 `shouldBe` pure expected
      it "intersects at the the point (1, 1)" $ do
        let l1 = Line (Point 2 1) (Vector 1 0)
            l2 = Line (Point 2 2) (Vector 1 1)
            expected = Point 1 1
        l1 `intersectionWith` l2 `shouldBe` pure expected
    it "returns another point of intersection of nonparallel lines" $ do
      let l1 = Line (Point 500 350) (Vector -0.9701425001453318 0.24253562503633294)
          l2 = Line (Point 500 450) (Vector 0.9701425001453318 0.24253562503633294)
          expected = Point 451.49 400
      l1 `intersectionWith` l2 `shouldBe` pure expected
