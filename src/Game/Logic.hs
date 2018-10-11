{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Description: Reexports game logic from various modules
-}
module Game.Logic
    ( module Game.Logic.Geometry
    , module Game.Logic.Patterns
    , module Game.Logic.Levels
    )
where

import Game.Logic.Geometry
import Game.Logic.Patterns
import Game.Logic.Levels
