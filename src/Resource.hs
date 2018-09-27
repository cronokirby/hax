{-|
Description: Contains utilities for loading resources such as Sprites
-}
module Resource
    (
    )
where

import Data.Function ((&))
import Control.Monad (mapM)
import Foreign.C.Types (CInt)
import qualified SDL -- Mainly imported for texture things
import System.IO (FilePath)


-- | Used to contain the data for a Sprite and all its animations
-- Each sprite in the sheet must have the same size in Width Height
data SpriteSheet = SpriteSheet !CInt !CInt !SDL.Texture

-- | Contains all the sprite data for the game
newtype SpriteData = SpriteData [SpriteSheet]

-- | Contains the necessary information to locate a Sprite's sheet
data SpriteLocation = SpriteLocation !CInt !CInt !FilePath


-- | Loads sprites from a list of locations into a renderer
loadSprites :: SDL.Renderer -> [SpriteLocation] -> IO SpriteData
loadSprites renderer = fmap SpriteData . mapM load
  where
    load (SpriteLocation w h path) =
        SDL.loadBMP path >>=
        SDL.createTextureFromSurface renderer &
        fmap (SpriteSheet w h)