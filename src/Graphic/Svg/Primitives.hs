module Graphic.Svg.Primitives where

import Data.Text qualified as T (intercalate, pack, replace, unpack)
import Geom2d.Circle
import Geom2d.Point
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Segment
import Geom2d.Size
import Graphic.Svg.Attributes (attrsToStr)

segment :: Segment -> [String] -> String -> String
segment seg attributes template =
  let replacedTemplate =
        T.replace "{{x1}}" (T.pack $ show seg.start.x)
          . T.replace "{{y1}}" (T.pack $ show seg.start.y)
          . T.replace "{{x2}}" (T.pack $ show seg.end.x)
          . T.replace "{{y2}}" (T.pack $ show seg.end.y)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ T.pack template
   in T.unpack replacedTemplate

rectangle :: Rect -> [String] -> String -> String
rectangle rect attributes template =
  let replacedTemplate =
        T.replace "{{x}}" (T.pack $ show rect.origin.x)
          . T.replace "{{y}}" (T.pack $ show rect.origin.y)
          . T.replace "{{width}}" (T.pack $ show rect.size.width)
          . T.replace "{{height}}" (T.pack $ show rect.size.height)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ T.pack template
   in T.unpack replacedTemplate

circle :: Circle -> [String] -> String -> String
circle circ attributes template =
  let replacedTemplate =
        T.replace "{{cx}}" (T.pack $ show circ.center.x)
          . T.replace "{{cy}}" (T.pack $ show circ.center.y)
          . T.replace "{{r}}" (T.pack $ show circ.radius)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ T.pack template
   in T.unpack replacedTemplate

polygon :: Polygon -> [String] -> String -> String
polygon pol attributes template =
  let replacedTemplate =
        T.replace "{{points}}" (formatPoints pol.vertices)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ T.pack template
      formatPoints points =
        T.intercalate
          " "
          [T.pack (show p.x) <> "," <> T.pack (show p.y) | p <- points]
   in T.unpack replacedTemplate
