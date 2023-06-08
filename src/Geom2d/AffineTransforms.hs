module Geom2d.AffineTransforms where

import Geom2d.AffineTransf
import Geom2d.Nums (R)
import Geom2d.Point

mkScale :: R -> R -> Point -> AffineTransform
mkScale sx sy center =
  defaultTransf
    { sx = sx,
      sy = sy,
      tx = center.x * (1.0 - sx),
      ty = center.y * (1.0 - sy)
    }

mkRotation :: R -> Point -> AffineTransform
mkRotation radians center =
  let oneMinusCos = 1.0 - cos radians
   in AffineTransform
        { sx = cos radians,
          sy = cos radians,
          tx = center.x * oneMinusCos + center.y * sin radians,
          ty = center.y * oneMinusCos - center.x * sin radians,
          shx = -(sin radians),
          shy = -(sin radians)
        }
