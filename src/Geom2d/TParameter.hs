module Geom2d.TParameter (mkTParameter, tMin, tMid, tMax) where

import Geom2d.Nums (R)

tMin :: R
tMin = 0.0

tMax :: R
tMax = 1.0

tMid :: R
tMid = 0.5

mkTParameter :: Double -> Either String Double
mkTParameter t
  | 0 <= t && t <= 1 = Right t
  | otherwise = Left "Parameter t must be in the interval [0, 1]"
