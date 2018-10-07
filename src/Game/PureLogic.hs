{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , Angle(..)
    , AngularV(..)
    , Shape(..)
    , Polarity(..)
    , Look(..)
    , switchPolarity
    , clamp
    , TimeLine
    , stepTimeLine
    , makeTimeLineOnce
    , makeTimeLineRepeat
    , LevelEvents(..)
    )
where

import Apecs
import Linear (V2(..), (^*))


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


-- | Represents the current shape of some entity
data Shape = SquareShape | TriangleShape

-- | Represents the current polarity of some entity
data Polarity = Pink | Blue

-- | Represents how some entity appears, based on shape, color, and scale
-- The size parameter is the width of whatever shape we have
data Look = Look Double Shape Polarity

instance Component Look where
    type Storage Look = Map Look

switchPolarity :: Look -> Look
switchPolarity (Look scale shape Pink) = Look scale shape Blue
switchPolarity (Look scale shape Blue) = Look scale shape Pink


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



-- | Represents the events that can occur in a level
data LevelEvents 
    = CreateEnemy Position Look -- ^ Create a new enemy at a position