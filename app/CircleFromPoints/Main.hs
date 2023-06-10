module Main where

import Input

main :: IO ()
main = do
  conf <- readConfig
  (ea, eb, ec) <- parsePoints
  case sequence [ea, eb, ec] of
    Left err -> putStrLn $ "Error: " ++ err
    Right [a, b, c] -> putStr $ unlines $ map show [a, b, c]
  putStr $ show conf
