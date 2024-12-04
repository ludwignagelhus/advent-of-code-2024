module Util where

-- Could actually use typeclass here??
type Coord = (Int, Int)

-- probably some propper matrix lingo would be better here.
addCoords :: Coord -> Coord -> Coord
addCoords (x,y) (xx, yy) = (x + xx, y + yy)
