module Main where

import Geom2d.Circles
import Input

main :: IO ()
main = do
  conf <- readConfig
  (ea, eb, ec) <- parsePoints
  case sequence [ea, eb, ec] of
    Left err -> putStrLn $ "Error: " ++ err
    Right [a, b, c] -> print $ mkCircleFromPoints a b c
  putStr $ show conf
