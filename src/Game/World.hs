{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Description: contains the things pertaining to the model of the game state
-}
module Game.World
    ( World
    , Game
    , worldWidth
    , worldHeight
    , initWorld
    , initialiseGame
    , stepGame
    )
where

import Apecs
import Control.Monad (forM_, unless, void, when)
import Data.Maybe (fromMaybe)
import Linear (V2(..), (^*))

import Game.Logic
import Game.Input


-- These are both constants to be used elsewhere
worldWidth :: Double
worldWidth = 600

worldHeight :: Double
worldHeight = 800

-- | Converts a direction into instantaneous speed in px/s
getSpeed :: Input -> Vec
getSpeed (Input _ _ slow (Direction lr ud)) = (* speed) <$>
    defaultDir (fmap lrSpeed lr) + defaultDir (fmap udSpeed ud)
  where
    speed = if getHeld slow then 160 else 340
    defaultDir = fromMaybe (V2 0 0)
    lrSpeed DLeft  = V2 (-1) 0
    lrSpeed DRight = V2 1 0
    udSpeed DUp    = V2 0 (-1)
    udSpeed DDown  = V2 0 1


{- | Represents whether or not this entity is the player.

The player has a double indicating rate of fire. This way of controlling
rate of fire is unique to the player, since enemies have more complicated
scripts, but the player is controlled uniquely by controller input.
-}
newtype Player = Player Double

instance Component Player where
    type Storage Player = Unique Player

-- | All the components associated with a player
type PlayerUnit = (Player, Visible)

{- | Represents the bullets fired by the player.

It's necessary to have a seperate type, since we want to make sure
that enemies only take damage from this type of bullet.
-}
data PlayerBullet = PlayerBullet

instance Component PlayerBullet where
    type Storage PlayerBullet = Map PlayerBullet

-- | All the components associated with a player bullet
type PlayerBulletUnit = (PlayerBullet, Visible)


type Unit 
    = Either 
      (Either BulletUnit PlayerBulletUnit) 
      (Either PlayerUnit EnemyUnit)

-- | Represents the global timeline for the game
newtype GlobalTimeLine = GlobalTimeLine (TimeLine LevelEvents)

instance Component GlobalTimeLine where
    type Storage GlobalTimeLine = Unique GlobalTimeLine


makeWorld "World"
    [ ''Position
    , ''Velocity
    , ''Angle
    , ''AngularV
    , ''Look
    , ''Health
    , ''Player
    , ''PlayerBullet
    , ''Bullet
    , ''BulletScript
    , ''Enemy
    , ''GlobalTimeLine
    , ''LevelState
    ]

type Game a = System World a


-- | Initialises the game state with an initial player position
initialiseGame :: Game ()
initialiseGame =
    let look = Look 28 SquareShape Pink
        pos = Position (V2 300 600)
        velocity = Velocity 0
    in void $ do
        newEntity (Player 0, (pos, velocity, look))
        newEntity (GlobalTimeLine mainLevel)
        set global (InLevel Pink 3 0)

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Input -> Game (LevelState, [(Position, Maybe Angle, Look)])
stepGame dT input = do
    -- Detect what state we're in based on the hud
    -- We might want to do this with some other mechanism
    hud <- get global
    case hud of
        NoLevel       -> return (NoLevel, [])
        (InLevel _ _ _) -> stepLevel dT input

-- | Advances the game logic while currently in a level.
stepLevel :: Double -> Input -> Game (LevelState, [(Position, Maybe Angle, Look)])
stepLevel dT input = do
    handleInput dT input
    handleScripts dT
    handleTimeLine dT
    stepKinetic dT
    stepSpinning dT
    clampPlayer
    handleCollisions
    deleteLowHealth
    deleteOffscreen
    floorScore
    entities <- getAll
    hud <- get global
    return (hud, entities)


-- | Changes the game based on the player's input
handleInput :: Double -> Input -> Game ()
handleInput dT input = do
    -- We need to potentially change the hud color
    cmapM (handlePlayer dT)
    when ((getHeld . inputShooting) input) . void $ cmapM shoot
  where
    -- Sets player speed, decrements reload counter, and switches polarity
    handlePlayer :: Double -> (Player, Velocity, Look) -> Game (Player, Velocity, Look)
    handlePlayer dT (Player reload, _, l) = do
        let speed = getSpeed input
            switchInput = getToggle (inputSwitch input)
            newLook@(Look _ _ p) = if switchInput 
                then switchPolarity l 
                else l
        modify global (setHudColor p)
        return (Player (reload - dT), Velocity speed, newLook)
    {- Creates a new bullet when the player can shoot, and resets
    their reload value if shot.
    -}
    shoot :: (Player, Look, Position) -> Game (Player, Look, Position)
    shoot all@(Player r, look, p) = if r <= 0
        then do
            makeBullet look p
            return (Player 0.1, look, p)
        else return all
    -- Creates a new bullet with the same color as Look above position
    makeBullet :: Look -> Position -> Game ()
    makeBullet (Look size _ polarity) (Position p) =
        let look = Look 14 SquareShape polarity
            velocity = Velocity (V2 0 (-800))
            position = Position (p - V2 0 size)
            angle = Angle 0
            angularV = AngularV 720
        in void $ newEntity (PlayerBullet, ((position, velocity), (angle, angularV), look))

-- | Steps the timeLine forward, and handles the events
handleTimeLine :: Double -> Game ()
handleTimeLine dT = cmapM $ \(GlobalTimeLine tl) -> do
    let (newTl, event) = stepTimeLine tl dT
    maybe (return ()) handleEvent event
    return (GlobalTimeLine newTl)
  where
    handleEvent (CreateEnemy enemy) = void $ newEntity enemy

-- | Steps forward all animation scripts, and creates entities for them
handleScripts :: Double -> Game ()
handleScripts dT = cmapM $ \(BulletScript tl) -> do
    let (newTl, patt) = stepTimeLine tl dT
    maybe (return ()) handlePattern patt
    return (BulletScript newTl)
  where
    handlePattern (BulletPattern units) = forM_ units newEntity

-- | Moves all kinetic objects forward
stepKinetic :: Double -> Game ()
stepKinetic dT = cmap $ \(pos, vel) ->
    (move dT vel pos, vel)

-- | Moves all spinning objects forward
stepSpinning :: Double -> Game ()
stepSpinning dT = cmap $ \(Angle a, AngularV v) ->
    (Angle (a + v * dT), AngularV v)

-- | Keeps player within bounds
clampPlayer :: Game ()
clampPlayer = cmap $ \(Player r, look@(Look size _ _), p) ->
    (Player r, look, clamp worldWidth worldHeight p)

-- | Handles collisions between bullets and enemies
handleCollisions :: Game ()
handleCollisions = do
    cmapM_ checkEnemies
    cmapM_ checkPlayer
  where
    checkEnemies :: (PlayerBullet, Position, Look, Entity) 
                 -> Game ()
    checkEnemies (_, posB, lookB@(Look _ _ colorB), etyB) =
        cmapM_ $ \(Enemy, posE, Health h, lookE@(Look _ _ colorE), etyE) ->
            when (collides (-14) (posB, lookB) (posE, lookE)) $ do
                destroy etyB (Proxy @Unit)
                let sameColor = colorB == colorE
                    newH = if sameColor then 1 else -1
                unless sameColor (incrementScore 1000)
                set etyE (Health (h + newH))
    checkPlayer :: (Bullet, Position, Look, Entity) -> Game ()
    checkPlayer (_, posB, lookB@(Look _ _ colorB), etyB) =
        cmapM_ $ \(Player _, posP, lookP@(Look _ _ colorP)) -> do
            let collidesAt d = collides d (posB, lookB) (posP, lookP)
                sameColor = colorB == colorP
            -- check for close call collision
            when (collidesAt (-18) && not sameColor) $
                incrementScore 1
            -- check for bad collision
            when (collidesAt (-26)) $ do
                destroy etyB (Proxy @Unit)
                if not sameColor
                    then decrementPlayerHealth
                    else incrementScore 250
                
{- | Decrement the health of the player in a level

This also affects the score of the player.
-}
decrementPlayerHealth :: Game ()
decrementPlayerHealth = modify global $
    \(InLevel p h s) -> InLevel p (h - 1) (s - 30000)

-- | Increment the score in a level
incrementScore :: Int -> Game ()
incrementScore toAdd = modify global $
    \(InLevel p h s) -> InLevel p h (s + toAdd)

-- | Removes all enemies with no Health
deleteLowHealth :: Game ()
deleteLowHealth = cmapM $ \e@(Enemy, Health h) ->
    if h <= 0
        then do -- Delete enemy and increment score
            incrementScore (10000)
            return (Left (Not @ EnemyUnit))
        else return (Right e)
    
-- | Deletes all visible particles whose position is offscreen
deleteOffscreen :: Game ()
deleteOffscreen = cmap delete
  where
    delete :: (Position, Look, Not Player) 
           -> Either (Position, Look) (Not Unit)
    delete (pos@(Position (V2 x y)), look@(Look size _ _), _) =
        if inBounds x worldWidth && inBounds y worldHeight
            then Left (pos, look)
            else Right Not
      where
        inBounds s mx = s - size <= mx && s + size >= 0


-- | Make sure score is at least 0
floorScore :: Game ()
floorScore = modify global $
    \(InLevel p i s) -> InLevel p i (max 0 s)
