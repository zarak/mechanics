module Geom2d.TParameter (mkTParameter) where

mkTParameter :: Double -> Either String Double
mkTParameter t
  | 0 <= t && t <= 1 = Right t
  | otherwise = Left "Parameter t must be in the interval [0, 1]"
