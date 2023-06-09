module Graphic.Svg.Primitives where

import Data.Text qualified as T (pack, replace, unpack)
import Geom2d.Point
import Geom2d.Segment
import Graphic.Svg.Attributes (attrsToStr)

segment :: Segment -> [String] -> String -> String
segment seg attributes template =
  let replacedTemplate =
        T.replace "{{x1}}" (T.pack $ show seg.start.x)
          . T.replace "{{y1}}" (T.pack $ show seg.start.y)
          . T.replace "{{x2}}" (T.pack $ show seg.start.x)
          . T.replace "{{y2}}" (T.pack $ show seg.end.y)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ T.pack template
   in T.unpack replacedTemplate
