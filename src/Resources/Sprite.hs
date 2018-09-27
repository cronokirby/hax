{-|
Description: Contains abstract data related to Sprites

This module also acts as a global source of truth with regards to
what sprites can exist. As such, it exports Sprite indices which
are always valid, and a mapping to file resources which must always
be kept in sync with the actual resources folder used in development.
-}
module Resources.Sprite
    (SpriteLocation(..)
    )
where

import Foreign.C.Types (CInt)
import System.IO (FilePath)


-- | Contains the necessary information to locate a Sprite's sheet
-- CInt is mainly used for conveniance of interaction with SDL
data SpriteLocation = SpriteLocation !CInt !CInt !FilePath


-- | The locations for sprites used in this project
-- This should always be kept in sync with the /resources folder
spriteLocations :: [SpriteLocation]
spriteLocations = map makeLocation
    [ (60, 60, "resources")
    ]
  where
    spritePath = "resources/sprites/"
    makeLocation (w, h, path) = SpriteLocation w h (spritePath ++ path)