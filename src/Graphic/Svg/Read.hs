module Graphic.Svg.Read where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (makeAbsolute)

readTemplate :: FilePath -> IO Text
readTemplate fileName = do
  absolutePath <- makeAbsolute ("src/Graphic/Svg/templates/" <> fileName)
  bytesStr <- BS.readFile absolutePath
  pure (decodeUtf8 bytesStr)
