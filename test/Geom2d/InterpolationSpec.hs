module Geom2d.InterpolationSpec where

import Geom2d.Interpolation
import Geom2d.Nums (R, areCloseEnough)
import Test.Hspec

spec :: Spec
spec = do
  describe "easeInOutTSequence" $ do
    it "creates a list of values between 0 and 1" $ do
      let expected = [0.0, 0.012195121951219514, 0.058823529411764705, 0.15517241379310345, 0.30769230769230776, 0.5, 0.6923076923076923, 0.8448275862068965, 0.9411764705882353, 0.9878048780487805, 1.0] :: [R]
          actual = easeInOutTSequence 10
          zipped = zip expected actual
          areClose = fmap (uncurry (areCloseEnough 1e-10)) zipped
      areClose `shouldSatisfy` all (== True)
