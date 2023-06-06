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

distanceSegmentToPoint :: Point -> Segment -> R
distanceSegmentToPoint p = distanceTo p . closestPoint p

intersectionWith :: Segment -> Segment -> Maybe Point
intersectionWith s1 s2
  | d1 `isParallelTo` d2 = Nothing
  | otherwise = case (mkTParameter t1, mkTParameter t2) of
      (Just t1', Just _) -> pure $ pointAt t1' s1
      (_, _) -> Nothing
  where
    d1 = directionVector s1
    d2 = directionVector s2
    crossProd = d1 `cross` d2
    delta = s2.start `subP` s1.start
    t1 = (delta.u * d2.v - delta.v * d2.u) / crossProd
    t2 = (delta.u * d1.v - delta.v * d1.u) / crossProd
