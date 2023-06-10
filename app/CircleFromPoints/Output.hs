{-# LANGUAGE OverloadedRecordDot #-}

module Output where

import Geom2d.Circle
import Geom2d.Point
import Geom2d.Vector
import Graphic.Svg.Attributes
import Graphic.Svg.Primitives (Template)
import Graphic.Svg.Primitives qualified as P
import Input

drawToSvg :: [Point] -> Circle -> Config -> IO ()
drawToSvg points circle config = print "almost there"

outputToSvg :: Circle -> Config -> Template -> [String]
outputToSvg circle config templates =
  let style = styleFromConfig config
      labelStyle = labelStyleFromConfig config
   in [ P.circle circle style templates._circle,
        P.text ("0 " <> show circle.center) circle.center (Vector 0 0) labelStyle templates._text,
        P.text ("r " <> show circle.radius) circle.center (Vector 0 20) labelStyle templates._text
      ]
