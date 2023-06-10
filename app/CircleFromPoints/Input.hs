module Input where

parsePoints :: IO (String, String, String)
parsePoints = do
  p1 <- getLine
  p2 <- getLine
  p3 <- getLine
  pure (p1, p2, p3)
