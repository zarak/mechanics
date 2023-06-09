module Graphic.Svg.Attributes where

import Geom2d.AffineTransf
import Geom2d.Nums (R)

strokeColor :: String -> String
strokeColor color = "stroke=\"" <> color <> "\""

strokeWidth :: R -> String
strokeWidth width = "stroke-width=\"" <> show width <> "\""

fillColor :: String -> String
fillColor color = "fill=\"" <> color <> "\""

fillOpacity :: R -> String
fillOpacity opacity = "fill-opacity=\"" <> show opacity <> "\""

affineTransform :: AffineTransform -> String
affineTransform t =
  let sxVal = show t.sx
      shyVal = show t.shy
      shxVal = show t.shx
      syVal = show t.sy
      txVal = show t.tx
      tyVal = show t.ty
      values = unwords [sxVal, shyVal, shxVal, syVal, txVal, tyVal]
   in "transform=\"matrix(" <> values <> ")\""

fontSize :: R -> String
fontSize size = "font-size=\"" <> show size <> "px\""

fontFamily :: String -> String
fontFamily font = "font-family=\"" <> font <> "\""

attrsToStr :: [String] -> String
attrsToStr = unwords
