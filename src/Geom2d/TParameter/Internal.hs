module Geom2d.TParameter.Internal where

import Geom2d.Nums (R, areCloseEnough)

newtype TParameter = UnsafeTParameter
  {unTParameter :: R}
  deriving (Show)

instance Eq TParameter where
  (UnsafeTParameter t1) == (UnsafeTParameter t2) = areCloseEnough tolerance t1 t2
    where
      tolerance = 1e-10
