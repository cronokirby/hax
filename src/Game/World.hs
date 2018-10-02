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
import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import Linear (V2(..), (*^), (^*))

import Game.Geometry
import Game.Input


-- These are both constants to be used elsewhere
worldWidth :: Double
worldWidth = 600

worldHeight :: Double
worldHeight = 800

-- | Converts a direction into instantaneous speed in px/s
getSpeed :: Input -> Vec
getSpeed (Input _ _ fast (Direction lr ud)) = (* speed) <$>
    defaultDir (fmap lrSpeed lr) + defaultDir (fmap udSpeed ud)
  where
    speed = if getHeld fast then 320 else 160
    defaultDir = fromMaybe (V2 0 0)
    lrSpeed DLeft  = V2 (-1) 0
    lrSpeed DRight = V2 1 0
    udSpeed DUp    = V2 0 (-1)
    udSpeed DDown  = V2 0 1


-- | Represents the current velocity of an entity in pixels/s
-- Entities with velocities should always have positions,
-- as the velocity acts on this position to make the entity move
newtype Velocity = Velocity Vec

instance Component Velocity where
    type Storage Velocity = Map Velocity

-- | Given a fraction of a second, and a velocity, advance a position
move :: Double -> Velocity -> Position -> Position
move dT (Velocity v) (Position p) = Position (p + v ^* dT)


type Kinetic = (Position, Velocity)
type Visible = (Look, Kinetic)

-- | Represents whether or not this entity is the player
-- Each player has a shooting rate
data Player = Player Double

instance Component Player where
    type Storage Player = Unique Player


makeWorld "World"
    [ ''Position
    , ''Velocity
    , ''Look
    , ''Player
    ]

type Game a = System World a


-- | Initialises the game state with an initial player position
initialiseGame :: Game ()
initialiseGame =
    let look = Look 36 SquareShape Pink
        pos = Position (V2 300 600)
        velocity = Velocity 0
    in void $ newEntity (Player 0, look, pos, velocity)

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Input -> Game [(Position, Look)]
stepGame dT input = do
    handleInput dT input
    stepKinetic dT
    clampPlayer
    deleteOffscreen
    getAll


-- | Changes the game based on the player's input
handleInput :: Double -> Input -> Game ()
handleInput dT input = do
    cmap (handlePlayer dT)
    when ((getHeld . inputShooting) input) . void $ cmapM shoot
  where
    handlePlayer :: Double -> (Player, Velocity, Look) -> (Player, Velocity, Look)
    handlePlayer dT (Player reload, _, l) =
        let speed = getSpeed input
            newLook = if (getToggle . inputSwitch) input
                then switchPolarity l
                else l
        in (Player (reload - dT), Velocity speed, newLook)
    shoot :: (Player, Look, Position) -> Game (Player, Look, Position)
    shoot all@(Player r, look, p) = do
        if r <= 0
            then do
                makeBullet look p
                return (Player 0.1, look, p)
            else return all
    makeBullet :: Look -> Position -> Game ()
    makeBullet (Look size _ polarity) (Position p) =
        let look = Look 16 SquareShape polarity
            velocity = Velocity (V2 0 (-800))
            position = Position (p - V2 0 size)
        in void $ newEntity (look, position, velocity)

-- | Keeps player within bounds
clampPlayer :: Game ()
clampPlayer = cmap $ \(Player r, look@(Look size _ _), p) ->
    (Player r, look, clamp worldWidth worldHeight p)


-- | Moves all kinetic objects forward
stepKinetic :: Double -> Game ()
stepKinetic dT = cmap $ \(Position p, Velocity v) ->
    (Position (p + v ^* dT), Velocity v)


-- | Deletes all visible particles whose position is offscreen
deleteOffscreen :: Game ()
deleteOffscreen = cmap delete
  where
    delete :: (Position, Look, Not Player) -> Either (Position, Look) (Not Visible)
    delete (pos@(Position (V2 x y)), look@(Look size _ _), _) =
        if inBounds x worldWidth && inBounds y worldHeight
            then Left (pos, look)
            else Right Not
      where
        inBounds s mx = s - size <= mx && s + size >= 0