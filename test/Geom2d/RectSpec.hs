module Geom2d.RectSpec (spec) where

import Geom2d.Point
import Geom2d.Rect
import Geom2d.Size
import Test.Hspec

spec :: Spec
spec = do
  describe "containsPoint" $ do
    context "if a point is inside the rectangle" $ do
      it "returns True" $ do
        let origin = Point 0 0
            size = Size 100 100
            rect = Rect origin size
            p = Point 50 50
        p `shouldSatisfy` containsPoint rect
    context "if a point is not inside the rectangle" $ do
      it "returns False" $ do
        let origin = Point 0 0
            size = Size 100 100
            rect = Rect origin size
            p = Point 150 50
        p `shouldNotSatisfy` containsPoint rect
