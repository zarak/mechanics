{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Circle where

import Geom2d.Nums (R)
import Geom2d.Point

data Circle = Circle
  { center :: Point,
    radius :: R
  }
  deriving (Show)

area :: Circle -> R
area circle = pi * circle.radius

circumference :: Circle -> R
circumference circle = 2 * pi * circle.radius
