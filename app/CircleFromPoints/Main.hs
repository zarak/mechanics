module Main where

import Data.Either (fromRight)
import Input

main :: IO ()
main = do
  (a, b, c) <- parsePoints
  let points = fmap (show . fromRight mempty) [a, b, c]
  putStr $ unlines points
