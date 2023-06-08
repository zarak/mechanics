module Geom2d.AffineTransf where

import Geom2d.Circle
import Geom2d.Circle qualified as Circle (toPolygon)
import Geom2d.Nums (R, areCloseEnough)
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

instance Eq AffineTransform where
  aff1 == aff2 =
    areCloseEnough tol aff1.sx aff2.sx
      && areCloseEnough tol aff1.sy aff2.sy
      && areCloseEnough tol aff1.tx aff2.tx
      && areCloseEnough tol aff1.ty aff2.ty
      && areCloseEnough tol aff1.shx aff2.shx
      && areCloseEnough tol aff1.shy aff2.shy
    where
      tol = 1e-10

defaultTransf :: AffineTransform
defaultTransf = AffineTransform {sx = 1, sy = 1, tx = 0, ty = 0, shx = 0, shy = 0}

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

affThen :: AffineTransform -> AffineTransform -> AffineTransform
affThen affT1 affT2 =
  AffineTransform
    { sx = affT2.sx * affT1.sx + affT2.shx * affT1.shy,
      sy = affT2.shy * affT1.shx + affT2.sy * affT1.sy,
      tx = affT2.sx * affT1.tx + affT2.shx * affT1.ty + affT2.tx,
      ty = affT2.shy * affT1.tx + affT2.sy * affT1.ty + affT2.ty,
      shx = affT2.sx * affT1.shx + affT2.shx * affT1.sy,
      shy = affT2.shy * affT1.sx + affT2.sy * affT1.shy
    }

inverse :: AffineTransform -> AffineTransform
inverse affT =
  let denom = affT.sx * affT.sy - affT.shx * affT.shy
   in AffineTransform
        { sx = affT.sy / denom,
          sy = affT.sx / denom,
          tx = (affT.ty * affT.shx - affT.sy * affT.tx) / denom,
          ty = (affT.tx * affT.shy - affT.sx * affT.ty) / denom,
          shx = -affT.shx / denom,
          shy = -affT.shy / denom
        }
