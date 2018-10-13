{-# LANGUAGE TypeFamilies #-}
{-|
Description: Describes timelines of events and bullets.

This module contains the logic necessary to be able to describe 
animations and more generally, timelines of events. It uses these
concepts to then talk about patterns of bullet fire.
-}
module Game.Logic.Patterns
    ( TimeLine
    , stepTimeLine
    , makeTimeLineOnce
    , makeTimeLineRepeat
    , Path
    , divide
    , cross
    , Bullet(..)
    , BulletUnit
    , BulletPattern(..)
    , pathWithLook
    , BulletScript(..)
    , noScript
    )
where

import Apecs (Component, Map, Storage)
import Linear (V2(..), (*^), angle)

import Game.Logic.Geometry


{- TimeLines -}

{- | Represents an abstract time line.

This is usually constructed via the other timeline types.
-}
newtype TimeLine a = TimeLine { stepTimeLine :: Double -> (TimeLine a, Maybe a) } 


{- | Represents a time line of actions that ocurr once.

When converted to a timeline, no actions will fire after
the end of the list of events is reached.
-}
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

{- | Make a timeline that runs a list of events a single time.

>>> tl = makeTimeLineOnce [(1.0, "Hello")]
>>> (newTl, event) = stepTimeLine tl 2.0
>>> event
Just "Hello"
>>> snd (stepTimeLine newTl 2.0)
Nothing
-}
makeTimeLineOnce :: [(Double, a)] -> TimeLine a
makeTimeLineOnce = timeLineOnce . OneTimeLine 0


-- | Represents a time line of actions that repeat.
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

{- | Makes a timeline that repeats a series of events.

Note that with the way this timeline works, once the end of the list
of events is reached, it takes one more step to reset the timeline.
This should have no effect on things, since timeSteps are so small.
This would lead to noticeable choppiness if timeSteps were on the order
of a second or so, but at that point, all the graphics would go to hell.

>>> tl = makeTimeLineRepeat [(1.0, "Hello")]
>>> (tl2, event) = stepTimeLine tl 2.0
>>> event
Just "Hello"
>>> (tl3, event) = (stepTimeLine tl2 2.0)
>>> event
Nothing
>>> snd (stepTimeLine tl3 2.0)
Just "Hello"
-}
makeTimeLineRepeat :: [(Double, a)] -> TimeLine a
makeTimeLineRepeat = timeLineRepeat . RepeatTimeLine 0 0


{- Paths -}

{- | Just the underlying movement of bullets composing a path.

Look and Spinning are excluded because those can be
applied independent of where a bullet is going.
-}
newtype Path = Path [Kinetic] deriving Show

instance Semigroup Path where
    (Path xs) <> (Path ys) = Path (xs ++ ys)

instance Monoid Path where
    mempty = Path []


{- | Evenly divide objects around a circle centered at a point.

This function produces unit vectors distributed counter clockwise,
at interval given by 2pi/spacing.

>>> divide 0 (Position 0)
Path []
>>> divide (-1) (Position 0)
Path []
>>> divide 1 (Position 0)
Path [(Position (V2 1.0 0.0),Velocity (V2 100.0 0.0))]
-}
divide :: Int -> Position -> Path
divide spacing (Position center) = Path $
    map (makeKinetic . angle) (angles spacing)
  where
    angles spacing
        | spacing <= 0 = []
        | otherwise    = 
            let ang = 2 * pi / fromIntegral spacing
            in map ((* ang) . fromIntegral) [0..spacing - 1]
    makeKinetic dir = (Position (center + dir), Velocity (100 * dir))



-- | Make a cross offset by a distance from a central point.
cross :: Double -> Position -> Path
cross offset (Position center) = Path . (<$> dirs) $ \dir ->
    (Position (center + offset *^ dir), Velocity (speed * dir))
  where
    speed = 100
    dirs = V2 <$> [-1, 1] <*> [-1, 1]


{- Bullets and BulletScripts-}

-- | Tags certain things as bullets.
data Bullet = Bullet deriving (Show)

instance Component Bullet where
    type Storage Bullet = Map Bullet

{- | All the components attached to a bullet.

This type is useful to make sure that all the components attached to
bullets are correctly deleted.
-}
type BulletUnit = (Bullet, Visible)

-- | Represents a shooting pattern for bullets.
newtype BulletPattern = BulletPattern [BulletUnit] deriving (Show)

-- | Adds a constant look to a pattern.
pathWithLook :: Spinning -> Look -> Path -> BulletPattern
pathWithLook spinning look (Path xs) = 
    BulletPattern $ map (\kin -> (Bullet, (kin, spinning, look))) xs


-- | Represents a timeline of bullet patterns to shoot.
newtype BulletScript = BulletScript (TimeLine BulletPattern)

instance Component BulletScript where
    type Storage BulletScript = Map BulletScript

{- | A bullet script where nothing happens.

>>> (BulletScript tl) = noScript
>>> snd (stepTimeLine tl 1.0)
Nothing
-}
noScript :: BulletScript
noScript = BulletScript (makeTimeLineOnce [])
