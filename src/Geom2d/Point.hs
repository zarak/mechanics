{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Point where

import Geom2d.Nums

data Point = Point {x :: R, y :: R}
  deriving (Show)

instance Eq Point where
  p1 == p2 =
    areCloseEnough 1e-10 p1.x p2.x
      && areCloseEnough 1e-10 p1.y p2.y

distanceTo :: Point -> Point -> R
distanceTo a b = sqrt $ deltaX ** 2 + deltaY ** 2
  where
    deltaX = x a - x b
    deltaY = y a - y b

test :: IO ()
test = do
  let p = Point 1 2
  print $ p.x

addP :: Point -> Point -> Point
addP (Point x1 y1) (Point x2 y2) = Point (x1 + y1) (x2 + y2)
