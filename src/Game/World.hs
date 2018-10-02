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
import Control.Monad (void)
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
    defaultDir = maybe (V2 0 0) id
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


-- | Represents whether or not this entity is the player
data Player = Player

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
    in void $ newEntity (Player, look, pos, velocity)

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Input -> Game [(Position, Look)]
stepGame dt input = do
    handlePlayer input
    stepKinetic dt
    clampPlayer
    getAll


handlePlayer :: Input -> Game ()
handlePlayer input = do
    cmap $ \(Player, Velocity _, l) ->
        let speed = getSpeed input
            newLook = if (getToggle . inputSwitch) input
                then switchPolarity l
                else l
        in (Player, Velocity speed, newLook)

clampPlayer :: Game ()
clampPlayer = cmap $ \(Player, p) ->
    (Player, clamp worldWidth worldHeight p)

stepKinetic :: Double -> Game ()
stepKinetic dT = cmap $ \(Position p, Velocity v) ->
    (Position (p + v ^* dT), Velocity v)