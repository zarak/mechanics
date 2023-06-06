module Geom2d.SegmentSpec (spec) where

import Geom2d.Point
import Geom2d.Segment
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
