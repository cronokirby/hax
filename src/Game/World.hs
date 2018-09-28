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
    ( Direction(..)
    , LRToggle(..)
    , World
    , Game
    , initWorld
    , initialiseGame
    , stepGame
    )
where

import Apecs
import Control.Monad (void)
import Linear (V2(..), (*^), (^*))


-- | Represents the secondary direction
data LRToggle = ToggleLeft | ToggleRight | ToggleStraight

-- | Represents the directions of control of a player
-- 8 way control, with Up and Down having an addition L/R toggle
data Direction = DUp LRToggle | DLeft | DRight | DDown LRToggle

-- | Converts a direction into instantaneous speed in px/s
directionSpeed :: Maybe Direction -> Vec
directionSpeed Nothing = 0
directionSpeed (Just d) = 20 *^ dir d
  where
    shift ToggleStraight = 0
    shift ToggleLeft     = V2 (-1) 0
    shift ToggleRight    = V2 1 0
    dir DLeft     = V2 (-1) 0
    dir DRight    = V2 1 0
    dir (DUp t)   = V2 0 (-1) + shift t
    dir (DDown t) = V2 0 1 + shift t


-- | Utility type used for both positions and action vectors
type Vec = V2 Double


{- Actual Apecs stuff -}

-- | Represents the current position of some entity
newtype Position = Position Vec

instance Component Position where
    type Storage Position = Map Position

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


makeWorld "World" [''Position, ''Velocity, ''Player]

type Game a = System World a


-- | Initialises the game state with an initial player position
initialiseGame :: Game ()
initialiseGame = void $ 
    newEntity (Player, Position playerPos, velocity)
  where
    playerPos = V2 300 600
    velocity = Velocity 0

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Maybe Direction -> Game Vec
stepGame dt playerDirection = do
    setPlayerSpeed playerDirection
    stepKinetic dt
    ((Player, Position p):_) <- getAll
    return p


setPlayerSpeed :: Maybe Direction -> Game ()
setPlayerSpeed direction = cmap $ \(Player, Velocity _) ->
    (Player, Velocity speed)
  where
    speed = directionSpeed direction

stepKinetic :: Double -> Game ()
stepKinetic dT = cmap $ \(Position p, Velocity v) ->
    (Position (p + v ^* dT), Velocity v)