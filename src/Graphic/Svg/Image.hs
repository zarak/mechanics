{-# LANGUAGE OverloadedStrings #-}

module Graphic.Svg.Image where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T (pack, replace, unpack)
import Geom2d.AffineTransf
import Geom2d.Point
import Geom2d.Rect
import Geom2d.Size
import Graphic.Svg.Primitives (imgTemplate)

defaultViewboxRect :: Size -> Rect
defaultViewboxRect = Rect mempty

viewboxFromRect :: Rect -> String
viewboxFromRect rect =
  let x = show rect.origin.x
      y = show rect.origin.y
      width = show rect.size.width
      height = show rect.size.height
   in unwords [x, y, width, height]

transfMatrixVals :: AffineTransform -> String
transfMatrixVals t =
  let sxVal = show t.sx
      shyVal = show t.shy
      shxVal = show t.shx
      syVal = show t.sy
      txVal = show t.tx
      tyVal = show t.ty
   in unwords [sxVal, shyVal, shxVal, syVal, txVal, tyVal]

svgContent ::
  Size ->
  [String] ->
  Maybe Rect ->
  Maybe AffineTransform ->
  String
svgContent size primitives viewboxRect transform = do
  let viewboxRect' = fromMaybe (defaultViewboxRect size) viewboxRect
      transform' = fromMaybe defaultTransf transform
      replacedTemplate =
        T.replace "{{width}}" (T.pack $ show $ width size)
          . T.replace "{{height}}" (T.pack $ show $ height size)
          . T.replace "{{content}}" (T.pack $ unlines primitives)
          . T.replace "{{viewBox}}" (T.pack $ viewboxFromRect viewboxRect')
          . T.replace "{{transf}}" (T.pack $ transfMatrixVals transform')
          $ imgTemplate
   in T.unpack replacedTemplate
