module Geom2d.TParameter (unTParameter, TParameter, mkTParameter, tMin, tMid, tMax) where

import Geom2d.Nums (R)

newtype TParameter = UnsafeTParameter
  {unTParameter :: R}
  deriving (Show, Eq)

tMin :: TParameter
tMin = UnsafeTParameter 0.0

tMax :: TParameter
tMax = UnsafeTParameter 1.0

tMid :: TParameter
tMid = UnsafeTParameter 0.5

mkTParameter :: Double -> Either String TParameter
mkTParameter t
  | 0 <= t && t <= 1 = Right $ UnsafeTParameter t
  | otherwise = Left "Parameter t must be in the interval [0, 1]"
