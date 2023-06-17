module Main where

import Geom2d.Circles
import Input
import Output

main :: IO ()
main = do
  conf <- readConfig
  (ea, eb, ec) <- parsePoints
  case sequence [ea, eb, ec] of
    Left err -> putStrLn $ "Error: " ++ err
    Right [a, b, c] -> do
      case mkCircleFromPoints a b c of
        Just circle -> drawToSvg [a, b, c] circle conf
        Nothing -> print "Could not create circle"
