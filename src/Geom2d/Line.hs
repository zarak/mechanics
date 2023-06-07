{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Line where

import Geom2d.Point (Point)
import Geom2d.Vector (Vector, perpendicular)
import Geom2d.Vector qualified as Vector (isParallelTo, isPerpendicularTo)

data Line = Line
  { base :: Point,
    direction :: Vector
  }
  deriving (Show)

isParallelTo :: Line -> Line -> Bool
isParallelTo l1 l2 = direction l1 `Vector.isParallelTo` direction l2

isPerpendicularTo :: Line -> Line -> Bool
isPerpendicularTo l1 l2 = direction l1 `Vector.isPerpendicularTo` direction l2

perpendicularThrough :: Point -> Line -> Line
perpendicularThrough p line =
  Line p (perpendicular line.direction)

parallelThrough :: Point -> Line -> Line
parallelThrough p line =
  Line p line.direction
