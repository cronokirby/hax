{-# LANGUAGE TypeFamilies #-}
{-|
Description: Contains data and types related to the shape of objects.

Instead of working with raw sprites, most objects in the game map to
simple geometric objects as well as colors. Having these contained here
allows us to keep collision logic close to where geometry is defined.

Having definitions of geometry is also necessary for drawing.
-}
module Game.Geometry
    ( Vec
    , Position(..)
    , Shape(..)
    , Polarity(..)
    , Look(..)
    , clamp
    )
where

import Apecs
import Linear (V2(..))


-- | Utility type since most of our vectors will look like this
type Vec = V2 Double

-- | Represents the current position of some entity
newtype Position = Position Vec

instance Component Position where
    type Storage Position = Map Position

-- | Clamps x between 0 and width, and y between 0 and height
clamp :: Double -> Double -> Position -> Position
clamp width height (Position (V2 x y)) =
    Position (V2 (clamp' width x) (clamp' height y))
  where
    clamp' mx val
      | val < 0  = 0
      | val > mx  = mx
      | otherwise = val

-- | Represents the current shape of some entity
data Shape = SquareShape | TriangleShape

-- | Represents the current polarity of some entity
data Polarity = Pink | Blue

-- | Represents how some entity appears, based on shape, color, and scale
data Look = Look Double Shape Polarity

instance Component Look where
    type Storage Look = Map Look