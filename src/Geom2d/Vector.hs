module Geom2d.Vector where

import Debug.Trace (trace)
import Geom2d.Nums
import Geom2d.Point
import Numeric.IEEE
import Prelude hiding (length)

data Vector = Vector {u :: R, v :: R}

instance Show Vector where
  show vector =
    "("
      <> show vector.u
      <> ", "
      <> show vector.v
      <> ") with norm "
      <> show (norm vector)

instance Eq Vector where
  v1 == v2 =
    areCloseEnough 1e-10 v1.u v2.u
      && areCloseEnough 1e-10 v1.v v2.v

subP :: Point -> Point -> Vector
subP (Point x1 y1) (Point x2 y2) = Vector (x1 - x2) (y1 - y2)

(^+^) :: Vector -> Vector -> Vector
Vector u1 v1 ^+^ Vector u2 v2 =
  Vector (u1 + u2) (v1 + v2)

(^-^) :: Vector -> Vector -> Vector
Vector u1 v1 ^-^ Vector u2 v2 = Vector (u1 - u2) (v1 - v2)

scaledBy :: R -> Vector -> Vector
scaledBy factor (Vector u v) = Vector (factor * u) (factor * v)

displaced :: R -> Vector -> Point -> Point
displaced times vector (Point x y) =
  let scaledVec = scaledBy times vector
   in trace ("displaced point: " <> show (x + scaledVec.u)) (Point (x + scaledVec.u) (y + scaledVec.v))

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
isPerpendicularTo v = isCloseToZero . dot v

-- | `angleValueBetween` computes \( \theta \)
-- \[
--    \theta = \acos \left( \frac{\vec u \cdot \vec v }{\Lvert \vec u \Rvert \cdot \Lvert \vec v \Rvert} \right)
-- \]
angleValueBetween :: Vector -> Vector -> R
angleValueBetween v1 v2 = acos (dotProduct / normProduct)
  where
    dotProduct = dot v1 v2
    normProduct = norm v1 * norm v2

angleTo :: Vector -> Vector -> R
angleTo v1 v2 = copySign value crossProduct
  where
    value = angleValueBetween v1 v2
    crossProduct = cross v1 v2

(>|) :: Vector -> Vector -> R
v1 >| v2 = angleTo v1 v2

infix 7 >|

rotatedRadians :: R -> Vector -> Vector
rotatedRadians radians vector =
  let c = cos radians
      s = sin radians
   in Vector
        (vector.u * c - vector.v * s)
        (vector.u * s + vector.v * c)

perpendicular :: Vector -> Vector
perpendicular (Vector u v) = Vector (-v) u

opposite :: Vector -> Vector
opposite = scaledBy (-1)

sine :: Vector -> R
sine vector = vector.v / norm vector

cosine :: Vector -> R
cosine vector = vector.u / norm vector
