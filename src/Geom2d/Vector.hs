{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Vector where

import Geom2d.Nums
import Geom2d.Point

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

displaced :: Point -> Vector -> R -> Point
displaced (Point x y) vector times =
  let scaledVec = scaledBy times vector
   in Point (x + scaledVec.u) (y + scaledVec.v)
