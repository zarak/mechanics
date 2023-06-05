{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Point where

import Geom2d.Nums

data Point = Point {x :: R, y :: R}
  deriving (Show)

data Vector = Vector {u :: R, v :: R}
  deriving (Show)

distance :: Point -> Point -> R
distance a b = sqrt $ deltaX ** 2 + deltaY ** 2
  where
    deltaX = x a - x b
    deltaY = y a - y b

test :: IO ()
test = do
  let p = Point 1 2
  print $ p.x

addP :: Point -> Point -> Point
addP (Point x1 y1) (Point x2 y2) = Point (x1 + y1) (x2 + y2)

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
