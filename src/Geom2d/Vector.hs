{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Vector where

import Geom2d.Nums
import Geom2d.Point
import Prelude hiding (length)

data Vector = Vector {u :: R, v :: R}
  deriving (Show)

subP :: Point -> Point -> Vector
subP (Point x1 y1) (Point x2 y2) = Vector (x1 - y1) (x2 - y2)

(^+^) :: Vector -> Vector -> Vector
Vector x1 y1 ^+^ Vector x2 y2 = Vector (x1 + y1) (x2 + y2)

(^-^) :: Vector -> Vector -> Vector
Vector x1 y1 ^-^ Vector x2 y2 = Vector (x1 - y1) (x2 - y2)

scaledBy :: R -> Vector -> Vector
scaledBy factor (Vector u v) = Vector (factor * u) (factor * v)

displaced :: R -> Vector -> Point -> Point
displaced times vector (Point x y) =
  let scaledVec = scaledBy times vector
   in Point (x + scaledVec.u) (y + scaledVec.v)

-- Properties
norm :: Vector -> R
norm vector = sqrt (vector.u ** 2 + vector.v ** 2)

isNormal :: Vector -> Bool
isNormal = isCloseToOne . norm

normalized :: Vector -> Vector
normalized vector = scaledBy (1.0 / norm vector) vector

withLength :: R -> Vector -> Vector
withLength length = scaledBy length . normalized
