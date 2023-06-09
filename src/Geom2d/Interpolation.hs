module Geom2d.Interpolation where

import Geom2d.Nums (R)
import Geom2d.TParameter (TParameter (..), unTParameter)

uniformTSequence :: R -> [TParameter]
uniformTSequence steps = UnsafeTParameter <$> [0, 1 / steps .. 1.0]

easeInOutT :: TParameter -> TParameter
easeInOutT t = UnsafeTParameter $ t' ** 2 / (t' ** 2 + (1 - t') ** 2)
  where
    t' = unTParameter t

easeInOutTSequence :: R -> [TParameter]
easeInOutTSequence = fmap easeInOutT . uniformTSequence

interpolate :: R -> R -> TParameter -> R
interpolate vs ve t = vs + unTParameter t * (ve - vs)
