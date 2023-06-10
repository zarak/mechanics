{-# LANGUAGE OverloadedRecordDot #-}

module Output where

import Geom2d.Circle
import Geom2d.Nums (R)
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
  let ptRadius = circle.radius / 20
      svgOutput = outputToSvg circle config.output templates
      svgInput = inputToSvg points ptRadius config.input templates
      viewbox = mkViewbox circle

      svgImg = svgContent viewbox.size (svgOutput <> svgInput) templates._img (Just viewbox) Nothing
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

inputToSvg :: [Point] -> R -> InputOutput -> Template -> [String]
inputToSvg points pointRadius config templates =
  let style = styleFromConfig config
      labelStyle = labelStyleFromConfig config
      [a, b, c] = points
      disp = Vector (1.25 * pointRadius) 0
   in [ P.circle (Circle a pointRadius) style templates._circle,
        P.circle (Circle b pointRadius) style templates._circle,
        P.circle (Circle c pointRadius) style templates._circle,
        P.text ("A " <> show a) a disp labelStyle templates._text,
        P.text ("B " <> show b) b disp labelStyle templates._text,
        P.text ("C " <> show c) c disp labelStyle templates._text
      ]
