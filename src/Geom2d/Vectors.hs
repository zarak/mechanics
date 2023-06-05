import Geom2d.Nums (R)
import Geom2d.Point (Point (..))
import Geom2d.Vector (Vector (..), normalized, subP)

mkVectorBetween :: Point -> Point -> Vector
mkVectorBetween p q = subP q p

mkVersor :: R -> R -> Vector
mkVersor u v = normalized $ Vector u v

mkVersorBetween :: Point -> Point -> Vector
mkVersorBetween p q = normalized $ mkVectorBetween p q
