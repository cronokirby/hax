{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Description: Contains data and types related to pure logic

Unlike Game.World, this module contains functions for
operating on things in a pure or geometric way.
We desribe timelines of actions without specifiying
the concrete game effects performed, or bullet patterns in
a pure way.
-}
module Game.PureLogic
    ( Vec
    , Position(..)
    , Velocity(..)
    , Kinetic
    , noVelocity
    , move
    , Angle(..)
    , AngularV(..)
    , Spinning
    , Shape(..)
    , Polarity(..)
    , Look(..)
    , switchPolarity
    , Visible
    , collides
    , clamp
    , TimeLine
    , stepTimeLine
    , makeTimeLineOnce
    , makeTimeLineRepeat
    , Health(..)
    , Bullet(..)
    , BulletUnit
    , BulletPattern(..)
    , BulletScript(..)
    , Enemy(..)
    , EnemyUnit
    , makeStaticEnemy
    , LevelEvents(..)
    , mainLevel
    )
where

import Data.Function ((&))

import Apecs
import Linear (V2(..), (^*), distance)


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


-- | Represents the current velocity of an entity in pixels/s
-- Entities with velocities should always have positions,
-- as the velocity acts on this position to make the entity move
newtype Velocity = Velocity Vec

instance Component Velocity where
    type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)

-- | Null velocity
noVelocity :: Velocity
noVelocity = Velocity (V2 0 0)

-- | Given a fraction of a second, and a velocity, advance a position
move :: Double -> Velocity -> Position -> Position
move dT (Velocity v) (Position p) = Position (p + v ^* dT)


-- | Represents the current angle of rotation in degrees
newtype Angle = Angle Double

instance Component Angle where
    type Storage Angle = Map Angle

-- | Represents the current change of angle, in degrees per second
newtype AngularV = AngularV Double

instance Component AngularV where
    type Storage AngularV = Map AngularV

type Spinning = (Angle, AngularV)


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


-- | Represents an abstract time line.
-- This is usually constructed via the other timeline types
newtype TimeLine a = TimeLine { stepTimeLine :: Double -> (TimeLine a, Maybe a) } 


-- | Represents a time line of actions that ocurr once.
-- When converted to a timeline, no actions will fire after
-- the end of the list of events is reached.
data OneTimeLine a = OneTimeLine Double [(Double, a)]

timeLineOnce :: OneTimeLine a -> TimeLine a
timeLineOnce tl@(OneTimeLine t []) = 
    TimeLine $ const (timeLineOnce tl, Nothing)
timeLineOnce (OneTimeLine t allEvents@((trigger, event):restEvents)) =
    TimeLine $ \dT ->
        let now = t + dT
            nextWith = timeLineOnce . OneTimeLine now
        in if now >= trigger
            then (nextWith restEvents, Just event)
            else (nextWith allEvents, Nothing)

-- | Make a timeline that runs a list of events a single time
makeTimeLineOnce :: [(Double, a)] -> TimeLine a
makeTimeLineOnce = timeLineOnce . OneTimeLine 0


-- | Represents a time line of actions that repeat
data RepeatTimeLine a = RepeatTimeLine Double Int [(Double, a)]

timeLineRepeat :: RepeatTimeLine a -> TimeLine a
timeLineRepeat (RepeatTimeLine t i l)
    | i >= length l || i < 0 = TimeLine $
        const (timeLineRepeat (RepeatTimeLine 0 0 l), Nothing)
    | otherwise = TimeLine $ \dT ->
        let now = t + dT 
            nextWith index = timeLineRepeat (RepeatTimeLine now index l)
            (trigger, event) = l !! i
        in if now >= trigger
            then (nextWith (i + 1), Just event)
            else (nextWith i, Nothing)

-- | Makes a timeline that repeats a series of events
makeTimeLineRepeat :: [(Double, a)] -> TimeLine a
makeTimeLineRepeat = timeLineRepeat . RepeatTimeLine 0 0


newtype Health = Health Int

instance Component Health where
    type Storage Health = Map Health

-- | Tags certain things as bullets
data Bullet = Bullet

instance Component Bullet where
    type Storage Bullet = Map Bullet

-- | All the components attached to a bullet.
-- This type is useful to make sure that all the components attached to
-- bullets are correctly deleted.
type BulletUnit = (Bullet, Visible)

-- Note: the logic for bullet patterns will eventually get complicated enough
-- to warrant its own module

-- | Represents a shooting pattern for bullets
newtype BulletPattern = BulletPattern [BulletUnit]

-- | Represents a timeline of bullet patterns to shoot
newtype BulletScript = BulletScript (TimeLine BulletPattern)

instance Component BulletScript where
    type Storage BulletScript = Map BulletScript

noScript :: BulletScript
noScript = BulletScript (makeTimeLineOnce [])

-- | Tag a certain entity as an enemy
data Enemy = Enemy

instance Component Enemy where
    type Storage Enemy = Map Enemy

-- | All enemies should have (at least) these components
-- We use a unit type here to be able to make sure to delete everything
-- attached to an enemy-
type EnemyUnit = (Enemy, Health, Visible, BulletScript)

-- | Creates an enemy that isn't moving
makeStaticEnemy :: Position -> Look -> Health -> EnemyUnit
makeStaticEnemy pos look health = 
    let kinetic = (pos, noVelocity)
        spinning = (Angle 0, AngularV 0)
    in (Enemy, health, (kinetic, spinning, look), noScript)

-- | Modify the velocity of an enemy
enemyWithVelocity :: Velocity -> EnemyUnit -> EnemyUnit
enemyWithVelocity v (tag, h, ((pos, _), spinning, look), script) = 
    (tag, h, ((pos, v), spinning, look), script)

-- | Modify the angular velocity of an enemy
enemyWithRotation :: AngularV -> EnemyUnit -> EnemyUnit
enemyWithRotation omega (tag, h, (kinetic, (angle, _), look), script) =
    (tag, h, (kinetic, (angle, omega), look), script)

-- | Modify the script of an enemy
enemyWithScript :: BulletScript -> EnemyUnit -> EnemyUnit
enemyWithScript script (tag, h, visible, _) =
    (tag, h, visible, script)

-- | Represents the events that can occur in a level
data LevelEvents 
    = CreateEnemy EnemyUnit -- ^ Create a new enemy at a position


mainLevel :: TimeLine LevelEvents
mainLevel = makeTimeLineOnce
    [ (1, CreateEnemy (Enemy, enemyHealth, ((Position (V2 100 100), Velocity (V2 0 0)), (Angle 0, AngularV 0), enemyLook Pink), someScript))
    , (1.1, enemyPos (V2 500 100) Blue)
    ]
  where
    somePattern = BulletPattern 
        [(Bullet, ((Position (V2 100 120), Velocity (V2 0 100)), (Angle 0, AngularV 0), Look 14 SquareShape Pink))]
    someScript = BulletScript $ makeTimeLineOnce [(0.2, somePattern)]
    enemyLook = Look 28 SquareShape
    enemyHealth = Health 15
    enemyPos pos pol = 
        makeStaticEnemy (Position pos) (enemyLook pol) enemyHealth
        & enemyWithRotation (AngularV 120)
        & enemyWithScript someScript
        & CreateEnemy