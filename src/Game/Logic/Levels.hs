{-# LANGUAGE TypeFamilies #-}
module Game.Logic.Levels
    ( Health (..)
    , Enemy(..)
    , EnemyUnit
    , LevelEvents(..)
    , mainLevel
    )
where

import Apecs (Component, Map, Storage)
import Data.Function ((&))
import Linear (V2(..))

import Game.Logic.Geometry
import Game.Logic.Patterns


newtype Health = Health Int

instance Component Health where
    type Storage Health = Map Health

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
    , (1.4, enemyPos (V2 500 100) Blue)
    ]
  where
    somePattern = BulletPattern 
        [(Bullet, ((Position (V2 300 120), Velocity (V2 0 100)), (Angle 0, AngularV 0), Look 14 SquareShape Pink))]
    someScript = BulletScript $ makeTimeLineRepeat [(1, somePattern)]
    enemyLook = Look 28 SquareShape
    enemyHealth = Health 15
    enemyPos pos pol = 
        makeStaticEnemy (Position pos) (enemyLook pol) enemyHealth
        & enemyWithRotation (AngularV 120)
        & enemyWithScript someScript
        & CreateEnemy
