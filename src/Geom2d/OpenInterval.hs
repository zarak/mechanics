{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.OpenInterval
  ( OpenInterval,
    mkOpenInterval,
    contains,
    length,
    overlapsInterval,
  )
where

import Geom2d.Nums (R, areCloseEnough)
import Prelude hiding (length)

data OpenInterval = UnsafeOpenInterval
  { start :: R,
    end :: R
  }
  deriving (Show)

length :: OpenInterval -> R
length oi = oi.end - oi.start

contains :: OpenInterval -> R -> Bool
contains oi x = oi.start < x && x < oi.end

mkOpenInterval :: R -> R -> Maybe OpenInterval
mkOpenInterval start end
  | start > end = Nothing
  | otherwise = Just $ UnsafeOpenInterval start end

overlapsInterval :: OpenInterval -> OpenInterval -> Bool
overlapsInterval oi1 oi2 = equalIntervals || partialOverlap
  where
    partialOverlap =
      oi1
        `contains` oi2.start
        || oi1
        `contains` oi2.end
        || oi2
        `contains` oi1.start
        || oi2
        `contains` oi1.end
    equalIntervals =
      areCloseEnough tolerance oi1.start oi2.start
        && areCloseEnough tolerance oi1.end oi2.end
    tolerance = 1e-10
