module Utils.Pairs where

-- makeRoundPairs :: [b] -> [(b, b)]
-- makeRoundPairs xs =
--   let len = length xs
--    in [(xs !! i, xs !! ((i + 1) `mod` len)) | i <- [0 .. len - 1]]

mkRoundPairs :: forall a. [a] -> [(a, a)]
mkRoundPairs [] = []
mkRoundPairs lst = go lst
  where
    go :: [a] -> [(a, a)]
    go (x : y : rest) = (x, y) : go (y : rest)
    go [x] = [(x, head lst)]
    go [] = []
