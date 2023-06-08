module Geom2d.OpenIntervalSpec where

import Geom2d.OpenInterval
import Test.Hspec

spec :: Spec
spec = do
  describe "overlapsInterval" $ do
    context "when the start and end points are equal" $ do
      it "should be True" $ do
        let oi = mkOpenInterval 0 10
        overlapsInterval <$> oi <*> oi `shouldBe` Just True
