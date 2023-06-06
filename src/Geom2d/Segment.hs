{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Segment where

import Geom2d.Nums (R)
import Geom2d.Point
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

pointAt :: R -> Segment -> Point
pointAt t segment = displaced t direction segment.start
  where
    direction = directionVector segment

middle :: Segment -> Point
middle = pointAt 0.5
