{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Rects where

import Geom2d.Nums (R)
import Geom2d.Point (Point (Point))
import Geom2d.Point qualified as Point (Point (..))
import Geom2d.Rect (Rect (..))
import Geom2d.Size (Size (..))

mkRectContaining :: [Point] -> Maybe Rect
mkRectContaining points
  | null points = Nothing
  | otherwise =
      let minX = minimum [p.x | p <- points]
          maxX = maximum [p.x | p <- points]
          minY = minimum [p.y | p <- points]
          maxY = maximum [p.y | p <- points]
       in pure $ Rect (Point minX minY) (Size (maxX - minX) (maxY - minX))

mkRectContainingWithMargin :: [Point] -> R -> Maybe Rect
mkRectContainingWithMargin points margin = do
  rect <- mkRectContaining points
  let point =
        Point
          (rect.origin.x - margin)
          (rect.origin.y - margin)
      size =
        Size
          (rect.size.width + 2 * margin)
          (rect.size.height + 2 * margin)
  pure $ Rect point size

mkRectCentered :: Point -> R -> R -> Rect
mkRectCentered center width height =
  let origin =
        Point
          (center.x - width / 2)
          (center.y - height / 2)
   in Rect origin (Size width height)
