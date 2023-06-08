{-# LANGUAGE OverloadedRecordDot #-}

module Geom2d.Size where

import Geom2d.Nums (R, areCloseEnough)

data Size = Size
  { width :: R,
    height :: R
  }
  deriving (Show)

instance Eq Size where
  (Size w1 h1) == (Size w2 h2) =
    areCloseEnough tolerance w1 w2
      && areCloseEnough tolerance h1 h2
    where
      tolerance = 1e-10
