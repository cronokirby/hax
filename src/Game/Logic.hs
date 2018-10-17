{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Description: Reexports game logic from various modules
-}
module Game.Logic
    ( module Game.Logic.Geometry
    , module Game.Logic.Patterns
    , module Game.Logic.Levels
    , RenderInfo(..)
    )
where

import Game.Logic.Geometry
import Game.Logic.Patterns
import Game.Logic.Levels


-- | Represents what needs to be rendered
data RenderInfo = RenderInfo 
    { renderLevelState :: LevelState
    , renderEntities :: [(Position, Maybe Angle, Look)]
    , renderEffect :: ScreenEffect
    }
