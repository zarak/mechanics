{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Polygon where

import Geom2d.Point
import Geom2d.Segment (Segment (..))
import Utils.Pairs (mkRoundPairs)

newtype Polygon = UnsafePolygon
  { vertices :: [Point]
  }
  deriving (Show)

mkPolygon :: [Point] -> Maybe Polygon
mkPolygon vertices
  | length vertices < 3 = Nothing
  | otherwise = pure $ UnsafePolygon vertices

sides :: Polygon -> [Segment]
sides poly =
  let vertexPairs = mkRoundPairs poly.vertices
   in [Segment s e | (s, e) <- vertexPairs]
