{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Rect where

import Geom2d.Nums (R)
import Geom2d.OpenInterval
import Geom2d.OpenInterval qualified as OpenInterval (length)
import Geom2d.Point (Point (..))
import Geom2d.Size

data Rect = Rect
  { origin :: Point,
    size :: Size
  }
  deriving (Show)

left :: Rect -> R
left rect = rect.origin.x

right :: Rect -> R
right rect = rect.origin.x + rect.size.width

bottom :: Rect -> R
bottom rect = rect.origin.y

top :: Rect -> R
top rect = rect.origin.y + rect.size.height

area :: Rect -> R
area rect = rect.size.width * rect.size.height

perimeter :: Rect -> R
perimeter rect = 2 * rect.size.width + 2 * rect.size.width

containsPoint :: Rect -> Point -> Bool
containsPoint rect p =
  ( left rect < p.x
      && p.x < right rect
  )
    && ( bottom rect < p.y
           && p.y < top rect
       )

intersectionWith :: Rect -> Rect -> Maybe Rect
intersectionWith r1 r2 = do
  hOverlap <- horizontalOverlapWith r1 r2
  vOverlap <- verticalOverlapWith r1 r2
  pure $
    Rect
      (Point hOverlap.start vOverlap.start)
      (Size (OpenInterval.length hOverlap) (OpenInterval.length vOverlap))
  where
    horizontalOverlapWith r1' r2' = do
      hInterval1 <- mkOpenInterval (left r1') (right r1')
      hInterval2 <- mkOpenInterval (left r2') (right r2')
      computeOverlapWith hInterval1 hInterval2
    verticalOverlapWith r1' r2' = do
      vInterval1 <- mkOpenInterval (bottom r1') (top r1')
      vInterval2 <- mkOpenInterval (bottom r2') (top r2')
      computeOverlapWith vInterval1 vInterval2
