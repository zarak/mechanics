{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Point where

import Geom2d.Nums

data Point = Point {x :: R, y :: R}
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

(^+^) :: Point -> Point -> Point
Point x1 y1 ^+^ Point x2 y2 = Point (x1 + y1) (x2 + y2)

(^-^) :: Point -> Point -> Point
Point x1 y1 ^-^ Point x2 y2 = Point (x1 - y1) (x2 - y2)
