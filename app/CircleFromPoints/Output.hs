{-# LANGUAGE OverloadedRecordDot #-}

module Output where

import Geom2d.Circle
import Geom2d.Point
import Geom2d.Rect
import Geom2d.Rects (mkRectCentered)
import Geom2d.Vector
import Graphic.Svg.Attributes qualified as A
import Graphic.Svg.Image (svgContent)
import Graphic.Svg.Primitives (Template)
import Graphic.Svg.Primitives qualified as P
import Input

drawToSvg :: [Point] -> Circle -> Config -> Template -> IO ()
drawToSvg points circle config templates = do
  let svgOutput = outputToSvg circle config.output templates
      viewbox = mkViewbox circle

      svgImg = svgContent viewbox.size svgOutput templates._img (Just viewbox) Nothing
  putStr svgImg

outputToSvg :: Circle -> InputOutput -> Template -> [String]
outputToSvg circle config templates =
  let style = styleFromConfig config
      labelStyle = labelStyleFromConfig config
   in [ P.circle circle style templates._circle,
        P.text ("0 " <> show circle.center) circle.center (Vector 0 0) labelStyle templates._text,
        P.text ("r " <> show circle.radius) circle.center (Vector 0 20) labelStyle templates._text
      ]

mkViewbox :: Circle -> Rect
mkViewbox circle =
  let height = 2.5 * circle.radius
      width = 4 * circle.radius
   in mkRectCentered circle.center width height

styleFromConfig :: InputOutput -> [String]
styleFromConfig config =
  [ A.strokeColor config.strokeColor,
    A.strokeWidth (fromIntegral config.strokeWidth),
    A.fillColor config.fillColor
  ]

labelStyleFromConfig :: InputOutput -> [String]
labelStyleFromConfig config =
  [ A.fontSize (fromIntegral config.labelSize),
    A.fontFamily config.fontFamily,
    A.fillColor config.strokeColor
  ]
