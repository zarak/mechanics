module Geom2d.TParameter (TParameter (..), mkTParameter, tMin, tMid, tMax) where

import Geom2d.Nums (R, areCloseEnough)

newtype TParameter = UnsafeTParameter
  {unTParameter :: R}
  deriving (Show)

instance Eq TParameter where
  (UnsafeTParameter t1) == (UnsafeTParameter t2) = areCloseEnough tolerance t1 t2
    where
      tolerance = 1e-10

tMin :: TParameter
tMin = UnsafeTParameter 0.0

tMax :: TParameter
tMax = UnsafeTParameter 1.0

tMid :: TParameter
tMid = UnsafeTParameter 0.5

mkTParameter :: Double -> Maybe TParameter
mkTParameter t
  | 0 <= t && t <= 1 = Just $ UnsafeTParameter t
  | otherwise = Nothing
