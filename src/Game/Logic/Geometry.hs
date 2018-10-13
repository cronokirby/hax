{-# LANGUAGE TypeFamilies #-}
{-|
Description: Talks about the shapes and kinetics of entities.

This exports pure logic to talk about geometry. Other Logic modules
build upon this one in order to describe bullets, enemies, etc.
-}
module Game.Logic.Geometry
    ( Vec
    , Position(..)
    , position
    , clamp
    , Velocity(..)
    , velocity
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


{- Straight Movement -}

-- | Utility type since most of our vectors will look like this
type Vec = V2 Double


-- | Represents the current position of some entity
newtype Position = Position Vec deriving (Eq, Show)

instance Component Position where
    type Storage Position = Map Position

-- | Creates a position given an x and a y component.
position :: Double -> Double -> Position
position x y = Position (V2 x y)

-- | Clamps x between 0 and width, and y between 0 and height
clamp :: Double -> Double -> Position -> Position
clamp width height (Position (V2 x y)) =
    Position (V2 (clamp' width x) (clamp' height y))
  where
    clamp' mx val
      | val < 0  = 0
      | val > mx  = mx
      | otherwise = val


{- | Represents the current velocity of an entity in pixels/s.

Entities with velocities should always have positions,
as the velocity acts on this position to make the entity move.
-}
newtype Velocity = Velocity Vec deriving (Eq, Show)

instance Component Velocity where
    type Storage Velocity = Map Velocity

-- | Creates a velocity given an x and a y component.
velocity :: Double -> Double -> Velocity
velocity x y = Velocity (V2 x y)

-- | Null velocity
noVelocity :: Velocity
noVelocity = Velocity (V2 0 0)


-- | Given a fraction of a second, and a velocity, advance a position.
move :: Double -> Velocity -> Position -> Position
move dT (Velocity v) (Position p) = Position (p + v ^* dT)


type Kinetic = (Position, Velocity)


{- Angular movement -}

-- | Represents the current angle of rotation in degrees.
newtype Angle = Angle Double deriving (Show)

instance Component Angle where
    type Storage Angle = Map Angle


-- | Represents the current change of angle, in degrees per second.
newtype AngularV = AngularV Double deriving (Show)

instance Component AngularV where
    type Storage AngularV = Map AngularV


type Spinning = (Angle, AngularV)


{- How things look -}

-- | Represents the current shape of some entity.
data Shape = SquareShape | TriangleShape deriving (Show)


-- | Represents the current polarity of some entity.
data Polarity = Pink | Blue deriving (Eq, Show)


{- | Represents how some entity appears, based on shape, color, and scale.

The size parameter is the width of whatever shape we have
-}
data Look = Look Double Shape Polarity deriving (Show)

instance Component Look where
    type Storage Look = Map Look

switchPolarity :: Look -> Look
switchPolarity (Look scale shape Pink) = Look scale shape Blue
switchPolarity (Look scale shape Blue) = Look scale shape Pink


-- | Visible items can move and spin
type Visible = (Kinetic, Spinning, Look)


{- | Detects a collision between 2 entities with positions and looks.

Detects collision as if both entities were circles, which is sufficient
given the polygonal shape of our sprites.
-}
collides :: Double -> (Position, Look) -> (Position, Look) -> Bool
collides infl (Position pos1, (Look w1 _ _)) (Position pos2, (Look w2 _ _)) = 
    distance pos1 pos2 - infl <= (w1 + w2)
