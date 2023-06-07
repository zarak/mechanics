module Geom2d.Polygons where

import Geom2d.Nums (R)
import Geom2d.Point (Point (..))
import Geom2d.Polygon (Polygon, mkPolygon)

-- mkPolygonFromCoords :: [R] -> Maybe Polygon
-- mkPolygonFromCoords coords
--   | odd (length coords) = Nothing
--   | otherwise = mkPolygon points
--   where
--     indices = [0, 2 .. length coords]
--     points = [Point (coords !! i) (coords !! i + 1) | i <- indices]

mkPolygonFromCoords :: [R] -> Maybe Polygon
mkPolygonFromCoords coords = mkPoints coords >>= mkPolygon
  where
    -- The odd check is redundant because the mkPoints function will only pattern
    -- match successfully on an even list.
    --
    -- \| odd (length coords) = Nothing
    -- \| otherwise = makePoints coords >>= mkPolygon

    mkPoints :: [R] -> Maybe [Point]
    mkPoints (x : y : rest) = (Point x y :) <$> mkPoints rest
    mkPoints [] = Just []
    mkPoints _ = Nothing
