{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Input where

import Data.Void
import Geom2d.Nums (R)
import Geom2d.Point
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L (decimal)

pInt :: Parsec Void String R
pInt = L.decimal

pPoint :: Parsec Void String Point
pPoint = do
  x <- pInt
  space
  y <- pInt
  pure (Point x y)

pointFromString :: String -> Either String Point
pointFromString str = case parse pPoint "" str of
  Left err -> Left $ errorBundlePretty err
  Right point -> Right point

parsePoints :: IO (Either String Point, Either String Point, Either String Point)
parsePoints = do
  p1 <- pointFromString <$> getLine
  p2 <- pointFromString <$> getLine
  p3 <- pointFromString <$> getLine
  pure (p1, p2, p3)
