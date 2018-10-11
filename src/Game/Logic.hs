{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Description: Contains data and types related to pure logic

Unlike Game.World, this module contains functions for
operating on things in a pure or geometric way.
We desribe timelines of actions without specifiying
the concrete game effects performed, or bullet patterns in
a pure way.
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
