module Geom2d.Line where

import Geom2d.Point (Point)
import Geom2d.Vector (Vector (..), cross, displaced, perpendicular)
import Geom2d.Vector qualified as Vector (isParallelTo, isPerpendicularTo)
import Geom2d.Vectors (mkVectorBetween)

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

intersectionWith :: Line -> Line -> Maybe Point
intersectionWith l1 l2
  | l1 `isParallelTo` l2 = Nothing
  | otherwise = pure $ displaced t1 d1 l1.base
  where
    d1 = l1.direction
    d2 = l2.direction
    crossProd = d1 `cross` d2
    delta = mkVectorBetween l1.base l2.base
    t1 = (delta.u * d2.v - delta.v * d2.u) / crossProd
