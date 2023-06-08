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
    context "when the first interval is to the left of the second" $ do
      it "should be False" $ do
        let oi1 = mkOpenInterval 0 10
            oi2 = mkOpenInterval 10 20
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just False
    context "when the first interval is to the right of the second" $ do
      it "should be False" $ do
        let oi1 = mkOpenInterval 21 30
            oi2 = mkOpenInterval 10 20
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just False
    context "when the first interval's endpoint is inside the second" $ do
      it "should be True" $ do
        let oi1 = mkOpenInterval 0 10
            oi2 = mkOpenInterval 5 20
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just True
    context "when the first interval's starting point is inside the second" $ do
      it "should be True" $ do
        let oi1 = mkOpenInterval 10 30
            oi2 = mkOpenInterval 5 20
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just True
    context "when the first interval is inside the second" $ do
      it "should be True" $ do
        let oi1 = mkOpenInterval 10 15
            oi2 = mkOpenInterval 5 20
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just True
    context "when the second interval is inside the first" $ do
      it "should be True" $ do
        let oi1 = mkOpenInterval 10 15
            oi2 = mkOpenInterval 12 13
        overlapsInterval <$> oi1 <*> oi2 `shouldBe` Just True
