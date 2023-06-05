module Geom2d.Nums where

type R = Double

areCloseEnough :: R -> R -> R -> Bool
areCloseEnough a b tolerance = abs (a - b) < tolerance

isCloseToZero :: R -> Bool
isCloseToZero a = areCloseEnough a 0 1e-10

isCloseToOne :: R -> Bool
isCloseToOne a = areCloseEnough a 1 1e-10
