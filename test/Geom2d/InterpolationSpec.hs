module Geom2d.InterpolationSpec where

import Geom2d.Interpolation
import Geom2d.TParameter
import Geom2d.TParameter.Internal (TParameter (UnsafeTParameter))
import Test.Hspec

spec :: Spec
spec = do
  describe "easeInOutTSequence" $ do
    it "creates a list of values between 0 and 1" $ do
      let expected = UnsafeTParameter <$> [0.0, 0.012195121951219514, 0.058823529411764705, 0.15517241379310345, 0.30769230769230776, 0.5, 0.6923076923076923, 0.8448275862068965, 0.9411764705882353, 0.9878048780487805, 1.0] :: [TParameter]
          actual = easeInOutTSequence 10
      expected `shouldBe` actual
