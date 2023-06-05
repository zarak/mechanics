module Geom2d.Nums where

type R = Double

areCloseEnough :: R -> R -> R -> Bool
areCloseEnough tolerance a b = abs (a - b) < tolerance

isCloseToZero :: R -> Bool
isCloseToZero a = areCloseEnough 1e-10 a 0

isCloseToOne :: R -> Bool
isCloseToOne a = areCloseEnough 1e-10 a 1
