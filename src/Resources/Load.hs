{-|
Description: Contains utilities for loading resources such as Sprites
-}
module Resources.Load
    ( SpriteData
    , loadProjectSprites
    , renderSprite
    )
where

import Data.Function ((&))
import Control.Monad (mapM)
import Foreign.C.Types (CInt)
import qualified SDL -- Mainly imported for texture things
import SDL (Rectangle(..), Point(..), V2(..))

import Resources.Sprite


-- | Used to contain the data for a Sprite and all its animations
-- Each sprite in the sheet must have the same size in Width Height
data SpriteSheet = SpriteSheet !CInt !CInt !SDL.Texture

sheetTexture :: SpriteSheet -> SDL.Texture
sheetTexture (SpriteSheet _ _ texture) = texture

-- | Returns the source rectangle given the index of a spritesheet
getFrame :: SpriteSheet -> Int -> Maybe (Rectangle CInt)
getFrame (SpriteSheet w h texture) index = Just $
    Rectangle (P (V2 0 (w * fromIntegral index))) (V2 w h)


-- | Contains all the sprite data for the game
newtype SpriteData = SpriteData [SpriteSheet]


-- | Loads sprites from a list of locations into a renderer
loadSprites :: SDL.Renderer -> [SpriteLocation] -> IO SpriteData
loadSprites renderer = fmap SpriteData . mapM load
  where
    load (SpriteLocation w h path) =
        SDL.loadBMP path >>=
        SDL.createTextureFromSurface renderer &
        fmap (SpriteSheet w h)

-- | Loads the sprites using the locations in Resources.Sprite
loadProjectSprites :: SDL.Renderer -> IO SpriteData
loadProjectSprites r = loadSprites r spriteLocations


-- | Renders a sprite from sprite data, given an index
renderSprite :: SpriteData -> SpriteIndex -> Maybe (Rectangle CInt)
             -> SDL.Renderer -> IO ()
renderSprite (SpriteData sheets) index dest r =
    let (RawIndex sheetPos frame) = getRaw index
        -- Always in the list, because we're keeping locations in sync
        sheet = sheets !! sheetPos
        source = getFrame sheet frame
    in SDL.copy r (sheetTexture sheet) source dest