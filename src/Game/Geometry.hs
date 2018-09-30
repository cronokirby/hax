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
    )
where

import Apecs
import Linear (V2)


-- | Utility type since most of our vectors will look like this
type Vec = V2 Double

-- | Represents the current position of some entity
newtype Position = Position Vec

instance Component Position where
    type Storage Position = Map Position

-- | Represents the current shape of some entity
data Shape = SquareShape

-- | Represents the current polarity of some entity
data Polarity = Pink

-- | Represents how some entity appears, based on shape, color
data Look = Look Shape Polarity

instance Component Look where
    type Storage Look = Map Look