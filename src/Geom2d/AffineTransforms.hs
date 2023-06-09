module Geom2d.AffineTransforms where

import Geom2d.AffineTransf
import Geom2d.Interpolation
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
          shy = sin radians
        }

mkRotationOrigin :: R -> AffineTransform
mkRotationOrigin = flip mkRotation (Point 0 0)

easeInOutInterpolation :: AffineTransform -> AffineTransform -> R -> [AffineTransform]
easeInOutInterpolation start end steps = [interpolated start end t | t <- tSeq]
  where
    tSeq = easeInOutTSequence steps
    interpolated s e t =
      AffineTransform
        { sx = interpolate s.sx e.sx t,
          sy = interpolate s.sy e.sy t,
          tx = interpolate s.tx e.tx t,
          ty = interpolate s.ty e.ty t,
          shx = interpolate s.shx e.shx t,
          shy = interpolate s.shy e.shy t
        }
