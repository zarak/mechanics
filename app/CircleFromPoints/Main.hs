module Main where

import Input

main :: IO ()
main = do
  (a, b, c) <- parsePoints
  putStr $ unlines [a, b, c]
