{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Segment where

import Geom2d.Point
import Geom2d.Vector
import Geom2d.Vectors

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
