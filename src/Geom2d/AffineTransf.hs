module Geom2d.AffineTransf where

import Geom2d.Circle
import Geom2d.Circle qualified as Circle (toPolygon)
import Geom2d.Nums (R)
import Geom2d.Point (Point (..))
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Rect qualified as Rect (toPolygon)
import Geom2d.Segment

data AffineTransform = AffineTransform
  { sx :: R,
    sy :: R,
    tx :: R,
    ty :: R,
    shx :: R,
    shy :: R
  }
  deriving (Show)

defaultTransf :: AffineTransform
defaultTransf = AffineTransform 1 1 0 0 0 0

applyToPoint :: AffineTransform -> Point -> Point
applyToPoint affT p =
  Point
    ((affT.sx * p.x) + (affT.shx * p.y) + affT.tx)
    ((affT.shy * p.x) + (affT.sy * p.y) + affT.ty)

applyToSegment :: AffineTransform -> Segment -> Segment
applyToSegment affT (Segment start end) =
  Segment (applyToPoint affT start) (applyToPoint affT end)

applyToPolygon :: AffineTransform -> Polygon -> Maybe Polygon
applyToPolygon affT polygon = mkPolygon transformedPoints
  where
    transformedPoints = fmap (applyToPoint affT) (vertices polygon)

applyToRect :: AffineTransform -> Rect -> Maybe Polygon
applyToRect affT rect = Rect.toPolygon rect >>= applyToPolygon affT

applyToCircle :: AffineTransform -> Circle -> R -> Maybe Polygon
applyToCircle affT circle divisions = Circle.toPolygon circle divisions >>= applyToPolygon affT

applyToCircle30 :: AffineTransform -> Circle -> Maybe Polygon
applyToCircle30 affT circle = applyToCircle affT circle 30
