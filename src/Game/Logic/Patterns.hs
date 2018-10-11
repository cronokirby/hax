module Game.Logic.Patterns
    ( TimeLine
    , stepTimeLine
    , makeTimeLineOnce
    , makeTimeLineRepeat
    )
where


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
