{-|
Description: Contains utility functions for drawing sprites
-}
module Drawing
    (drawSprites
    )
where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CDouble(..), CInt)
import qualified SDL
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))

import Game.Geometry
import Resources.Sprite


-- | Gets the sprite index corresponding to a given look
spriteIndex :: Look -> SpriteIndex
spriteIndex (Look _ SquareShape Pink)   = SpSquarePink
spriteIndex (Look _ SquareShape Blue)   = SpSquareBlue
spriteIndex (Look _ TriangleShape Pink) = SpTrianglePink
spriteIndex (Look _ TriangleShape Blue) = SpTriangleBlue

-- | Gets the destination rectangle for a sprite, given a central position
getDestination :: Position -> Look -> SpriteSheet -> Maybe (Rectangle CInt)
getDestination (Position pos) (Look width _ _) (SpriteSheet w h texture) =
    let shift = V2 width (width / fromIntegral w * fromIntegral h)
        topLeft = pos - fmap (/ 2) shift
    in Just . fmap round $ Rectangle (P topLeft) shift


-- | Renders a Look to a position given data about sprites and a renderer
renderLook :: Position -> Angle -> Look -> SpriteData -> SDL.Renderer -> IO ()
renderLook position (Angle alpha) look sprites renderer =
    let index = spriteIndex look
        (sheet, source) = getSprite sprites index
        dest = getDestination position look sheet
        toFlip = V2 False False
        angle = CDouble alpha
    in SDL.copyEx renderer (sheetTexture sheet) source dest angle Nothing toFlip

-- | Clears the background
clearScreen :: SDL.Renderer -> IO ()
clearScreen renderer = do
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 255


-- | Draws all the sprites, including the background
drawSprites :: [(Position, Maybe Angle, Look)] -> SpriteData -> SDL.Renderer -> IO ()
drawSprites toDraw sprites renderer = do
    clearScreen renderer
    forM toDraw $ \(pos, angle, look) ->
        renderLook pos (fromMaybe (Angle 0) angle) look sprites renderer
    SDL.present renderer