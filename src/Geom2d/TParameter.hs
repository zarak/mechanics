module Geom2d.TParameter (TParameter (unTParameter), mkTParameter, tMin, tMid, tMax) where

import Geom2d.Nums (R)
import Geom2d.TParameter.Internal

tMin :: TParameter
tMin = UnsafeTParameter 0.0

tMax :: TParameter
tMax = UnsafeTParameter 1.0

tMid :: TParameter
tMid = UnsafeTParameter 0.5

mkTParameter :: R -> Maybe TParameter
mkTParameter t
  | 0 <= t && t <= 1 = Just $ UnsafeTParameter t
  | otherwise = Nothing
