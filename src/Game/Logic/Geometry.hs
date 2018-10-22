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
    , oppositePolarity
    , polarityToColor
    , switchPolarity
    , Look(..)
    , Visible
    , collides
    , ScreenEffect(..)
    )
where

import Apecs (Component, Global, Map, Storage)
import Data.Word (Word8)
import Linear (V2(..), V4(..), (^*), distance)


{- Straight Movement -}

-- | Utility type since most of our vectors will look like this
type Vec = V2 Double


-- | Represents the current position of some entity
newtype Position = Position Vec deriving (Eq, Show)

instance Component Position where
    type Storage Position = Map Position

{- | Creates a position given an x and a y component.

>>> position 0.0 0.0
Position (V2 0.0 0.0)
-}
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

{- | Creates a velocity given an x and a y component.

>>> velocity 1.0 1.0
Velocity (V2 1.0 1.0)
-}
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
data Shape = SquareShape deriving (Show)


-- | Represents the current polarity of some entity.
data Polarity = Pink | Blue deriving (Eq, Show)

{- | Returns the polarity opposed to this one

>>> oppositePolarity Pink
Blue
>>> oppositePolarity Blue
Pink
-}
oppositePolarity :: Polarity -> Polarity
oppositePolarity Pink = Blue
oppositePolarity Blue = Pink

-- | Gives the corresponding RGBA color for a polarity
polarityToColor :: Polarity -> V4 Word8
polarityToColor Pink = V4 0xEA 0x44 0xB9 0xFF
polarityToColor Blue = V4 0x3E 0xC0 0xE0 0xFF



{- | Represents how some entity appears, based on shape, color, and scale.

The size parameter is the width of whatever shape we have
-}
data Look = Look Double Shape Polarity deriving (Show)

instance Component Look where
    type Storage Look = Map Look

{- | Switches the polarity of a look, but preserves every other component.

This is useful because we often want to swap what color something is,
without perturbing anything else.

>>> switchPolarity (Look 1.0 SquareShape Pink)
Look 1.0 SquareShape Blue
-}
switchPolarity :: Look -> Look
switchPolarity (Look scale shape p) =
    Look scale shape (oppositePolarity p)


-- | Visible items can move and spin
type Visible = (Kinetic, Spinning, Look)


{- | Detects a collision between 2 entities with positions and looks.

Detects collision as if both entities were circles, which is sufficient
given the polygonal shape of our sprites.
-}
collides :: Double -> (Position, Look) -> (Position, Look) -> Bool
collides infl (Position pos1, (Look w1 _ _)) (Position pos2, (Look w2 _ _)) = 
    distance pos1 pos2 - infl <= (w1 + w2)


-- | Represents global effects to the look of the screen
data ScreenEffect
    = ScreenShake
    | NoScreenEffect

instance Semigroup ScreenEffect where
    NoScreenEffect <> e = e
    e <> NoScreenEffect = e
    e <> _              = e

instance Monoid ScreenEffect where
    mempty = NoScreenEffect
    mappend = (<>)

instance Component ScreenEffect where
    type Storage ScreenEffect = Global ScreenEffect
