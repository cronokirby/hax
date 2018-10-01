{-|
Description: Contains utility functions for drawing sprites
-}
module Drawing
    (renderLook
    )
where

import Foreign.C.Types (CInt)
import qualified SDL
import SDL (Rectangle(..), Point(..), V2(..))

import Game.Geometry
import Resources.Sprite


-- | Gets the sprite index corresponding to a given look
spriteIndex :: Look -> SpriteIndex
spriteIndex (Look _ SquareShape Pink) = SpSquare

-- | Gets the destination rectangle for a sprite, given a central position
getDestination :: Position -> Look -> SpriteSheet -> Maybe (Rectangle CInt)
getDestination (Position pos) (Look scale _ _) (SpriteSheet w h texture) =
    let shift = ((*scale) . fromIntegral) <$> V2 w h
        topLeft = pos - fmap (/ 2) shift
    in Just . fmap round $ Rectangle (P topLeft) shift


-- | Renders a Look to a position given data about sprites and a renderer
renderLook :: Position -> Look -> SpriteData -> SDL.Renderer -> IO ()
renderLook position look sprites renderer =
    let index = spriteIndex look
        (sheet, source) = getSprite sprites index
        dest = getDestination position look sheet
    in SDL.copy renderer (sheetTexture sheet) source dest