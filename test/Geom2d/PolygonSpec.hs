module Geom2d.PolygonSpec where

import Geom2d.Point (Point (..))
import Geom2d.Polygon (mkPolygon, sides)
import Geom2d.Segment (Segment (..))
import Test.Hspec

spec :: Spec
spec = do
  let vertices = [Point 0 0, Point 30 0, Point 0 30]
      polygon = mkPolygon vertices
  describe "sides" $ do
    it "test" $ do
      let expected =
            [ Segment (head vertices) (vertices !! 1),
              Segment (vertices !! 1) (vertices !! 2),
              Segment (vertices !! 2) (head vertices)
            ]
      sides <$> polygon `shouldBe` Just expected
