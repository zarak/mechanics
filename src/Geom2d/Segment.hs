{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Segment where

import Geom2d.Nums (R)
import Geom2d.Point
import Geom2d.TParameter
import Geom2d.Vector
import Geom2d.Vectors
import Prelude hiding (length)

data Segment = Segment
  { start :: Point,
    end :: Point
  }
  deriving (Show)

directionVector :: Segment -> Vector
directionVector segment = mkVectorBetween segment.start segment.end

directionVersor :: Segment -> Vector
directionVersor segment = mkVersorBetween segment.start segment.end

normalVersor :: Segment -> Vector
normalVersor = perpendicular . directionVersor

length :: Segment -> R
length segment = segment.start `distanceTo` segment.end

pointAt :: TParameter -> Segment -> Point
pointAt t segment = displaced (unTParameter t) direction segment.start
  where
    direction = directionVector segment

middle :: Segment -> Point
middle = pointAt tMid

closestPoint :: Point -> Segment -> Point
closestPoint p segment
  | vs < 0 = segment.start
  | vs > l = segment.end
  | otherwise = displaced vs d segment.start
  where
    v = mkVectorBetween segment.start p
    d = directionVersor segment
    vs = v `projectionOver` d
    l = length segment
