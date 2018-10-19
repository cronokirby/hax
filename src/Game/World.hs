{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
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
import Data.Function ((&))
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

-- | Represents the invincibility state of some entity
data Invincibility 
    -- | The entity will be invincible for the remaining time
    = Invincible Double
    | NotInvincible

instance Component Invincibility where
    type Storage Invincibility = Map Invincibility


-- | All the components associated with a player
type PlayerUnit = (Player, Invincibility, Visible)

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
        ParticleUnit 
        (Either 
            (Either BulletUnit PlayerBulletUnit) 
            (Either PlayerUnit EnemyUnit))

-- | Represents the current state of the timeline
data TimeLineState = TLPaused | TLRunning

{- | Represents the global timeline for the game

The timeline state is paused by request from the level, and unpaused
when enemies disappear.
-}
data GlobalTimeLine = GlobalTimeLine TimeLineState (TimeLine LevelEvents)

instance Component GlobalTimeLine where
    type Storage GlobalTimeLine = Unique GlobalTimeLine

{- | Allows the timing of state transitions

This is useful to transition to a different state of levels with a
certain delay; for example, we can delay the game over state
for a few seconds after the death of the player for dramatic effect
-}
data StateTransition
    = TransitioningTo LevelState Double
    | NotTransitioning

instance Component StateTransition where
    type Storage StateTransition = Unique StateTransition


makeWorld "World"
    [ ''Position
    , ''Velocity
    , ''Angle
    , ''AngularV
    , ''Look
    , ''Health
    , ''Player
    , ''Invincibility
    , ''PlayerBullet
    , ''Bullet
    , ''BulletScript
    , ''Particle
    , ''Enemy
    , ''GlobalTimeLine
    , ''StateTransition
    , ''LevelState
    , ''ScreenEffect
    ]

type Game a = System World a


-- | Initialises the game state with an initial player position
initialiseGame :: Game ()
initialiseGame =
    let look = Look 28 SquareShape Pink
        pos = Position (V2 300 600)
        velocity = Velocity 0
    in void $ do
        newEntity (Player 0, NotInvincible, (pos, velocity, look))
        newEntity (GlobalTimeLine TLRunning mainLevel)
        set global (InLevel Pink 3 0)

-- | Steps the game forward with a delta and player input
stepGame :: Double -> Input -> Game RenderInfo
stepGame dT input = do
    -- Detect what state we're in based on the hud
    -- We might want to do this with some other mechanism
    hud <- get global
    case hud of
        GameOver      -> return (RenderInfo GameOver [] NoScreenEffect)
        (InLevel _ _ _) -> stepLevel dT input
    

-- | Advances the game logic while currently in a level.
stepLevel :: Double -> Input -> Game RenderInfo
stepLevel dT input = do
    -- reset screen effect
    set global NoScreenEffect
    handleInput dT input
    handleScripts dT
    handleTimeLine dT
    unPauseTimeLine
    stepKinetic dT
    stepSpinning dT
    stepInvincibility dT
    stepAndKillParticles dT
    clampPlayer
    playerHit <- handleCollisions
    deleteLowHealth
    deleteOffscreen
    floorScore
    checkPlayerHealth
    -- This needs to be done at the end because we may change into
    -- a different level state
    stepStateTransition dT
    entities <- getAll
    hud <- get global
    effect <- get global
    return (RenderInfo hud entities effect)


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
handleTimeLine dT = cmapM $ \case
    g@(GlobalTimeLine TLPaused _) -> return g
    GlobalTimeLine TLRunning tl -> do
        let (newTl, event) = stepTimeLine tl dT
        tlState <- maybe (return TLRunning) handleEvent event
        return (GlobalTimeLine tlState newTl)
  where
    handleEvent (CreateEnemy enemy) = do
        void $ newEntity enemy
        return TLRunning
    handleEvent WaitForEnemies = return TLPaused

-- | Unpauses time line if no enemies are left
unPauseTimeLine :: Game ()
unPauseTimeLine = do
    enemies <- getAll
    when (allDead enemies) . cmap $
        \(GlobalTimeLine _ tl) -> GlobalTimeLine TLRunning tl
  where
    allDead :: [Enemy] -> Bool
    allDead = null

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


{- | Advances the invincibility state of entities

The remaining invincible time is decremented by the
time step, and if the time is up, the component
switches to not invincible.
-}
stepInvincibility :: Double -> Game ()
stepInvincibility dT = cmap doStep
  where
    doStep i = case i of
        NotInvincible -> NotInvincible
        Invincible d ->
            let now = d - dT
            in if now <= 0 
                then NotInvincible
                else Invincible now

{- | Advances the state transition.

This will also set the current state of the level to whatever is
in the unique transition.
-}
stepStateTransition :: Double -> Game ()
stepStateTransition dT = cmapM $ \case
    NotTransitioning -> return NotTransitioning
    TransitioningTo a d -> 
        let now = d - dT
        in if now <= 0
            then do
                set global a
                return NotTransitioning
            else
                return (TransitioningTo a now)


-- | Steps forward the lifetime of particles, killing when necessary
stepAndKillParticles :: Double -> Game ()
stepAndKillParticles dT = cmap $ \(Particle d) ->
    let now = d - dT
    in if now <= 0
        then Left (Not @ Unit)
        else Right (Particle now)


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
        cmapM_ $ \case
            (Player _, Invincible _, _, _, ety) -> return ()
            (Player _, NotInvincible, posP, lookP@(Look _ _ colorP), etyP) -> do
                let collidesAt d = collides d (posB, lookB) (posP, lookP)
                    sameColor = colorB == colorP
                -- check for close call collision
                when (collidesAt (-18) && not sameColor) $
                    incrementScore 1
                -- check for bad collision
                when (collidesAt (-26)) $ do
                    destroy etyB (Proxy @Unit)
                    if not sameColor
                        then do
                            set etyP (Invincible 0.5)
                            -- trigger a screenshake
                            set global ScreenShake
                            decrementPlayerHealth
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
deleteLowHealth = cmapM deleteEnemies
 where
    deleteEnemies :: (Enemy, Health, Position, Look) 
                  -> Game (Either (Not EnemyUnit) (Enemy, Health, Position, Look))
    deleteEnemies e@(Enemy, Health h, pos, look)
        | h <= 0   = do
            incrementScore 10000
            forM_ (deathParticles 0.4 pos look) newEntity
            return (Left (Not @ EnemyUnit))
        | otherwise = return (Right e)
    
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

-- | Set a game over if the player's health is <= 0
checkPlayerHealth :: Game ()
checkPlayerHealth = do
    (InLevel p h s) <- get global
    when (h <= 0) $ do
        -- We only want to start a transition if we're not already transitioning
        transition <- getAll :: Game [StateTransition]
        when (null transition) . void $
            newEntity (TransitioningTo GameOver 1)
        -- delete the player and all components attached to it
        -- create particles
        cmapM killPlayer
  where
    killPlayer :: (Player, Position, Look) -> Game (Not Unit)
    killPlayer (_, pos, look) = do
        forM_ (deathParticles 1 pos look) newEntity
        return Not
    