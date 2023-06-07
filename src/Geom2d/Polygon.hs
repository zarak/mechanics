{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}

module Geom2d.Polygon where

import Geom2d.Nums (areCloseEnough)
import Geom2d.Point
import Geom2d.Segment (Segment (..))
import Geom2d.Vector (angleTo)
import Geom2d.Vectors (mkVectorBetween)
import Utils.Pairs (mkRoundPairs)

newtype Polygon = UnsafePolygon
  { vertices :: [Point]
  }
  deriving (Show, Eq)

mkPolygon :: [Point] -> Maybe Polygon
mkPolygon vertices
  | length vertices < 3 = Nothing
  | otherwise = pure $ UnsafePolygon vertices

sides :: Polygon -> [Segment]
sides poly =
  let vertexPairs = mkRoundPairs poly.vertices
   in [Segment s e | (s, e) <- vertexPairs]

centroid :: Polygon -> Point
centroid poly =
  let vtxCount = fromIntegral $ length poly.vertices
      vtxSum = foldr (<>) mempty poly.vertices
   in Point (vtxSum.x / vtxCount) (vtxSum.y / vtxCount)

containsPoint :: Point -> Polygon -> Bool
containsPoint p poly
  | p `elem` poly.vertices = True
  | otherwise = areCloseEnough tolerance angleSum (2 * pi)
  where
    tolerance = 1e-10
    vecs = [mkVectorBetween p v | v <- poly.vertices]
    pairedVecs = mkRoundPairs vecs
    angleSum = foldr ((+) . uncurry angleTo) 0 pairedVecs
