{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Point where

import Geom2d.Nums

data Point = Point {x :: R, y :: R}
  deriving (Show)

distanceBetween :: Point -> Point -> R
distanceBetween a b = sqrt $ deltaX ** 2 + deltaY ** 2
  where
    deltaX = x a - x b
    deltaY = y a - y b

test :: IO ()
test = do
  let p = Point 1 2
  print $ p.x
