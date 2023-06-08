module Geom2d.Circle where

import Geom2d.Nums (R, areCloseEnough)
import Geom2d.Point
import Geom2d.Polygon (Polygon, mkPolygon)

data Circle = Circle
  { center :: Point,
    radius :: R
  }
  deriving (Show)

instance Eq Circle where
  c1 == c2 =
    c1.center == c2.center
      && areCloseEnough 1e-10 c1.radius c2.radius

area :: Circle -> R
area circle = pi * circle.radius ** 2

circumference :: Circle -> R
circumference circle = 2 * pi * circle.radius

containsPoint :: Circle -> Point -> Bool
containsPoint circle p = circle.center `distanceTo` p < circle.radius

toPolygon :: Circle -> R -> Maybe Polygon
toPolygon circle divisions =
  let angleDelta = 2 * pi / divisions
      pointAtAngle c angle =
        Point
          (c.center.x + c.radius * cos angle)
          (c.center.y + c.radius * sin angle)
   in mkPolygon [pointAtAngle circle (angleDelta * i) | i <- [0 .. divisions - 1]]
