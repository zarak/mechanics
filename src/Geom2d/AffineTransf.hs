module Geom2d.AffineTransf where

import Geom2d.Nums (R)
import Geom2d.Point (Point (..))

data AffineTransform = AffineTransform
  { sx :: R,
    sy :: R,
    tx :: R,
    ty :: R,
    shx :: R,
    shy :: R
  }
  deriving (Show)

identity :: AffineTransform
identity = AffineTransform 1 1 0 0 0 0

applyToPoint :: AffineTransform -> Point -> Point
applyToPoint affT p =
  Point
    ((affT.sx * p.x) + (affT.shx * p.y) + affT.tx)
    ((affT.shy * p.x) + (affT.sy * p.y) + affT.ty)
