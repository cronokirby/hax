{-# LANGUAGE TypeFamilies #-}
module Game.Logic.Geometry
    ( Vec
    , Position(..)
    , clamp
    , Velocity(..)
    , noVelocity
    , move
    , Kinetic
    , Angle(..)
    , AngularV(..)
    , Spinning
    , Shape(..)
    , Polarity(..)
    , switchPolarity
    , Look(..)
    , Visible
    , collides
    )
where

import Apecs (Component, Map, Storage)
import Linear (V2(..), (^*), distance)


-- | Utility type since most of our vectors will look like this
type Vec = V2 Double


{- Straight Movement -}

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


-- | Represents the current velocity of an entity in pixels/s
-- Entities with velocities should always have positions,
-- as the velocity acts on this position to make the entity move
newtype Velocity = Velocity Vec

instance Component Velocity where
    type Storage Velocity = Map Velocity

-- | Null velocity
noVelocity :: Velocity
noVelocity = Velocity (V2 0 0)


-- | Given a fraction of a second, and a velocity, advance a position
move :: Double -> Velocity -> Position -> Position
move dT (Velocity v) (Position p) = Position (p + v ^* dT)


type Kinetic = (Position, Velocity)


{- Angular movement -}

-- | Represents the current angle of rotation in degrees
newtype Angle = Angle Double

instance Component Angle where
    type Storage Angle = Map Angle


-- | Represents the current change of angle, in degrees per second
newtype AngularV = AngularV Double

instance Component AngularV where
    type Storage AngularV = Map AngularV


type Spinning = (Angle, AngularV)


{- How things look -}

-- | Represents the current shape of some entity
data Shape = SquareShape | TriangleShape


-- | Represents the current polarity of some entity
data Polarity = Pink | Blue deriving (Eq)


-- | Represents how some entity appears, based on shape, color, and scale
-- The size parameter is the width of whatever shape we have
data Look = Look Double Shape Polarity

instance Component Look where
    type Storage Look = Map Look

switchPolarity :: Look -> Look
switchPolarity (Look scale shape Pink) = Look scale shape Blue
switchPolarity (Look scale shape Blue) = Look scale shape Pink


-- | Visible items can move and spin
type Visible = (Kinetic, Spinning, Look)


-- | Circle based colision for simplicity
collides :: Double -> (Position, Look) -> (Position, Look) -> Bool
collides infl (Position pos1, (Look w1 _ _)) (Position pos2, (Look w2 _ _)) = 
    distance pos1 pos2 - infl <= (w1 + w2)
