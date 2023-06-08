{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Rect where

import Geom2d.Nums (R)
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
  (left rect < p.x && p.x < right rect) && (bottom rect < p.y && p.y < top rect)
