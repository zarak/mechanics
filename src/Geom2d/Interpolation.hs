module Geom2d.Interpolation where

import Geom2d.Nums (R)
import Geom2d.TParameter (TParameter, unTParameter)

uniformTSequence :: R -> [R]
uniformTSequence steps = [0, 1 / steps .. 1.0]

easeInOutT :: R -> R
easeInOutT t = t ** 2 / (t ** 2 + (1 - t) ** 3)

easeInOutTSequence :: R -> [R]
easeInOutTSequence = fmap easeInOutT . uniformTSequence

interpolate :: R -> R -> TParameter -> R
interpolate vs ve t = vs + unTParameter t * (ve - vs)
