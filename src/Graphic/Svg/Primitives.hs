{-# LANGUAGE QuasiQuotes #-}

module Graphic.Svg.Primitives where

import Data.Text (Text)
import Data.Text qualified as T (intercalate, pack, replace, unpack)
import Geom2d.Circle
import Geom2d.Nums (R)
import Geom2d.Point
import Geom2d.Polygon
import Geom2d.Rect
import Geom2d.Segment
import Geom2d.Size
import Geom2d.Vector
import Graphic.Svg.Attributes (attrsToStr)
import Text.RawString.QQ

lineTemplate :: Text
lineTemplate = [r|<line x1="{{x1}}" y1="{{y1}}" x2="{{x2}}" y2="{{y2}}" {{attrs}}/>|]

rectTemplate :: Text
rectTemplate = [r|<rect x="{{x}}" y="{{y}}" width="{{width}}" height="{{height}}" {{attrs}}/>|]

circleTemplate :: Text
circleTemplate = [r|<circle cx="{{cx}}" cy="{{cy}}" r="{{r}}" {{attrs}}/>|]

polygonTemplate :: Text
polygonTemplate = [r|<polygon points="{{points}}" {{attrs}}/>|]

polylineTemplate :: Text
polylineTemplate = [r|<polyline points="{{points}}" {{attrs}}/>|]

textTemplate :: Text
textTemplate =
  [r|<text x="{{x}}" y="{{y}}" dx="{{dx}}" dy="{{dy}}" {{attrs}}> 
  {{text}} 
</text>|]

groupTemplate :: Text
groupTemplate =
  [r|<g {{attrs}}>
  {{content}}
</g>|]

segment :: Segment -> [String] -> String
segment seg attributes =
  let replacedTemplate =
        T.replace "{{x1}}" (T.pack $ show seg.start.x)
          . T.replace "{{y1}}" (T.pack $ show seg.start.y)
          . T.replace "{{x2}}" (T.pack $ show seg.end.x)
          . T.replace "{{y2}}" (T.pack $ show seg.end.y)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ lineTemplate
   in T.unpack replacedTemplate

rectangle :: Rect -> [String] -> String
rectangle rect attributes =
  let replacedTemplate =
        T.replace "{{x}}" (T.pack $ show rect.origin.x)
          . T.replace "{{y}}" (T.pack $ show rect.origin.y)
          . T.replace "{{width}}" (T.pack $ show rect.size.width)
          . T.replace "{{height}}" (T.pack $ show rect.size.height)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ rectTemplate
   in T.unpack replacedTemplate

circle :: Circle -> [String] -> String
circle circ attributes =
  let replacedTemplate =
        T.replace "{{cx}}" (T.pack $ show circ.center.x)
          . T.replace "{{cy}}" (T.pack $ show circ.center.y)
          . T.replace "{{r}}" (T.pack $ show circ.radius)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ circleTemplate
   in T.unpack replacedTemplate

formatPoints :: [Point] -> Text
formatPoints points =
  T.intercalate
    " "
    [T.pack (show p.x) <> "," <> T.pack (show p.y) | p <- points]

polygon :: Polygon -> [String] -> String
polygon pol attributes =
  let replacedTemplate =
        T.replace "{{points}}" (formatPoints pol.vertices)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ polygonTemplate
   in T.unpack replacedTemplate

polyline :: [Point] -> [String] -> String
polyline points attributes =
  let replacedTemplate =
        T.replace "{{points}}" (formatPoints points)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ polylineTemplate
   in T.unpack replacedTemplate

text :: String -> Point -> Vector -> [String] -> String
text txt pos disp attributes =
  let replacedTemplate =
        T.replace "{{x}}" (T.pack $ show pos.x)
          . T.replace "{{y}}" (T.pack $ show pos.y)
          . T.replace "{{dx}}" (T.pack $ show disp.u)
          . T.replace "{{dy}}" (T.pack $ show disp.v)
          . T.replace "{{text}}" (T.pack txt)
          . T.replace "{{attrs}}" (T.pack $ attrsToStr attributes)
          $ textTemplate
   in T.unpack replacedTemplate

group :: [String] -> [String] -> String
group primitives attributes =
  let replacedTemplate =
        T.replace
          "{{content}}"
          (T.intercalate "\n  " (T.pack <$> primitives))
          . T.replace
            "{{attrs}}"
            (T.pack $ attrsToStr attributes)
          $ groupTemplate
   in T.unpack replacedTemplate

arrow :: Segment -> R -> R -> [String] -> String
arrow seg arrowLength height attributes =
  let director = directionVector seg
      -- TODO: refactor withLength order of args so that we can write
      -- arrowLength `withLength` (opposite director)
      -- etc.
      v_l = withLength arrowLength (opposite director)
      v_h1 = withLength (height / 2.0) (perpendicular director)
      v_h2 = opposite v_h1
      p1 = displaced 1 (v_l ^+^ v_h1) seg.end
      p2 = seg.end
      p3 = displaced 1 (v_l ^+^ v_h2) seg.end
      poly = polyline [p1, p2, p3] []
   in group [segment seg [], poly] attributes
