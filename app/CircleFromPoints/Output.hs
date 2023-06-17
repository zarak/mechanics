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
import Graphic.Svg.Primitives qualified as P
import Input

drawToSvg :: [Point] -> Circle -> Config -> IO ()
drawToSvg points circle config = do
  let ptRadius = circle.radius / 20
      svgOutput = outputToSvg circle config.output
      svgInput = inputToSvg points ptRadius config.input
      viewbox = mkViewbox circle

      svgImg = svgContent viewbox.size (svgOutput <> svgInput) (Just viewbox) Nothing
  putStr svgImg

outputToSvg :: Circle -> InputOutput -> [String]
outputToSvg circle config =
  let style = styleFromConfig config
      labelStyle = labelStyleFromConfig config
   in [ P.circle circle style,
        P.text ("0 " <> show circle.center) circle.center (Vector 0 0) labelStyle,
        P.text ("r " <> show circle.radius) circle.center (Vector 0 20) labelStyle
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

inputToSvg :: [Point] -> R -> InputOutput -> [String]
inputToSvg points pointRadius config =
  let style = styleFromConfig config
      labelStyle = labelStyleFromConfig config
      [a, b, c] = points
      disp = Vector (1.25 * pointRadius) 0
   in [ P.circle (Circle a pointRadius) style,
        P.circle (Circle b pointRadius) style,
        P.circle (Circle c pointRadius) style,
        P.text ("A " <> show a) a disp labelStyle,
        P.text ("B " <> show b) b disp labelStyle,
        P.text ("C " <> show c) c disp labelStyle
      ]
