module Geom2d.Circles where

import Geom2d.Circle
import Geom2d.Line
import Geom2d.Point (Point, distanceTo)
import Geom2d.Segment (Segment (..), bisector)

mkCircleFromPoints :: Point -> Point -> Point -> Maybe Circle
mkCircleFromPoints p1 p2 p3 = do
  let chordOneBisec = bisector $ Segment p1 p2
      chordTwoBisec = bisector $ Segment p2 p3
  center <- chordOneBisec `intersectionWith` chordTwoBisec
  let radius = distanceTo center p1
  pure $ Circle center radius
