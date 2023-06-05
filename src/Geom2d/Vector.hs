{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Vector where

import Geom2d.Nums
import Geom2d.Point
import Numeric.IEEE
import Prelude hiding (length)

data Vector = Vector {u :: R, v :: R}
  deriving (Show, Eq)

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

dot :: Vector -> Vector -> R
dot v1 v2 = v1.u * v2.u + v1.v * v2.v

projectionOver :: Vector -> Vector -> R
projectionOver vector = dot vector . normalized

cross :: Vector -> Vector -> R
cross v1 v2 = v1.u * v2.v - v1.v * v2.u

isParallelTo :: Vector -> Vector -> Bool
isParallelTo v1 = isCloseToZero . cross v1

isPerpendicularTo :: Vector -> Vector -> Bool
isPerpendicularTo v1 = isCloseToZero . dot v1

-- | `angleValueBetween` computes \( \theta \)
-- \[
--    \theta = \acos \left( \frac{\vec u \cdot \vec v }{\Lvert \vec u \Rvert \cdot \Lvert \vec v \Rvert} \right)
-- \]
angleValueBetween :: Vector -> Vector -> R
angleValueBetween v1 v2 = acos (dotProduct / normProduct)
  where
    dotProduct = dot v1 v2
    normProduct = norm v1 * norm v2

angleBetween :: Vector -> Vector -> R
angleBetween v1 v2 = copySign value crossProduct
  where
    value = angleValueBetween v1 v2
    crossProduct = cross v1 v2

(>|) :: Vector -> Vector -> R
v1 >| v2 = angleBetween v1 v2

infix 7 >|

rotatedRadians :: Vector -> R -> Vector
rotatedRadians vector radians =
  Vector
    (vector.u * c - vector.v * s)
    (vector.u * s + vector.v * c)
  where
    c = cos radians
    s = sin radians
