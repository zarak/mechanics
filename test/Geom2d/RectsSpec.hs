{-# LANGUAGE NegativeLiterals #-}

module Geom2d.RectsSpec (spec) where

import Geom2d.Point
import Geom2d.Rect
import Geom2d.Rects
import Geom2d.Size
import Test.Hspec

spec :: Spec
spec = do
  let points = [Point 0 5, Point 10 0, Point 5 7]
  describe "mkRectContaining" $ do
    it "creates a Rect around a given list of points" $ do
      let expected = Rect (Point 0 0) (Size 10 7)
      mkRectContaining points `shouldBe` pure expected
  describe "mkRectContainingWithMargin" $ do
    it "creates a Rect around a given list of points with a margin" $ do
      let expected = Rect (Point -1 -1) (Size 12 9)
      mkRectContainingWithMargin points 1 `shouldBe` pure expected
  describe "mkRectCentered" $ do
    it "centers a rectangle around a given point" $ do
      let expected = Rect (Point -5 -10) (Size 20 40)
      mkRectCentered (Point 5 10) 20 40 `shouldBe` expected
