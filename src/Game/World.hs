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
import Control.Monad (forM_, void, when)
import Data.Maybe (fromMaybe)
import Linear (V2(..), (^*))

import Game.PureLogic
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


-- | Represents whether or not this entity is the player
-- Each player has a shooting rate
newtype Player = Player Double

instance Component Player where
    type Storage Player = Unique Player

-- | All the components associated with a player
type PlayerUnit = (Player, Visible)

type Unit = Either BulletUnit (Either PlayerUnit EnemyUnit)

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
    , ''Bullet
    , ''BulletScript
    , ''Enemy
    , ''GlobalTimeLine
    ]

type Game a = System World a


-- | Initialises the game state with an initial player position
initialiseGame :: Game ()
initialiseGame =
    let look = Look 28 SquareShape Pink
        pos = Position (V2 300 600)
        velocity = Velocity 0
        somePattern = BulletPattern 
            [(Bullet, ((Position (V2 100 120), Velocity (V2 0 100)), (Angle 0, AngularV 0), Look 14 SquareShape Pink))]
        someScript = BulletScript $ makeTimeLineOnce [(0.2, somePattern)]
    in void $ do
        newEntity (Player 0, (pos, velocity, look))
        --newEntity (GlobalTimeLine mainLevel)
        newEntity someScript

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Input -> Game [(Position, Maybe Angle, Look)]
stepGame dT input = do
    handleInput dT input
    handleTimeLine dT
    handleScripts dT
    stepKinetic dT
    stepSpinning dT
    clampPlayer
    handleCollisions
    deleteLowHealth
    deleteOffscreen
    getAll


-- | Changes the game based on the player's input
handleInput :: Double -> Input -> Game ()
handleInput dT input = do
    cmap (handlePlayer dT)
    when ((getHeld . inputShooting) input) . void $ cmapM shoot
  where
    -- Sets player speed, decrements reload counter, and switches polarity
    handlePlayer :: Double -> (Player, Velocity, Look) -> (Player, Velocity, Look)
    handlePlayer dT (Player reload, _, l) =
        let speed = getSpeed input
            newLook = if (getToggle . inputSwitch) input
                then switchPolarity l
                else l
        in (Player (reload - dT), Velocity speed, newLook)
    -- Creates a new bullet when the player can shoot, and resets
    -- there reload value if shot.
    shoot :: (Player, Look, Position) -> Game (Player, Look, Position)
    shoot all@(Player r, look, p) = if r <= 0
        then do
            makeBullet look p
            return (Player 0.1, look, p)
        else return all
    -- Creates a new bullet with the same color as Look above position
    makeBullet :: Look -> Position -> Game ()
    makeBullet (Look size _ polarity) (Position p) =
        let look = Look 12 SquareShape polarity
            velocity = Velocity (V2 0 (-800))
            position = Position (p - V2 0 size)
            angle = Angle 0
            angularV = AngularV 720
        in void $ newEntity (Bullet, ((position, velocity), (angle, angularV), look))

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
handleCollisions = cmapM_ doCollide
  where
    doCollide :: (Bullet, Position, Look, Entity) 
              -> Game ()
    doCollide (_, pos, lookB@(Look _ _ colorB), etyB) =
        cmapM_ $ \(Enemy, posE, Health h, lookE@(Look _ _ colorE), etyE) ->
            when (collides (-14) (pos, lookB) (posE, lookE)) $ do
                destroy etyB (Proxy @Unit)
                let newH = if colorB == colorE then 1 else -1
                set etyE (Health (h + newH))

-- | Removes all enemies with no Health
deleteLowHealth :: Game ()
deleteLowHealth = cmap $ \e@(Enemy, Health h) ->
    if h <= 0
        then Left (Not @ EnemyUnit)
        else Right e
    
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