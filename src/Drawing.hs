{-|
Description: Contains utility functions for drawing sprites
-}
module Drawing
    ( draw
    )
where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CDouble(..), CInt)
import qualified SDL
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))

import Game.Logic ( Position(..), Angle(..), Look(..), Polarity(..)
                  , Shape(..), Hud(..))
import Resources


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
renderLook :: Position -> Angle -> Look -> Resources -> SDL.Renderer -> IO ()
renderLook position (Angle alpha) look resources renderer =
    let index = spriteIndex look
        (sheet, source) = getSprite resources index
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
draw :: (Hud, [(Position, Maybe Angle, Look)]) -> Resources -> SDL.Renderer -> IO ()
draw (hud, toDraw) resources renderer = do
    clearScreen renderer
    forM_ toDraw $ \(pos, angle, look) ->
        renderLook pos (fromMaybe (Angle 0) angle) look resources renderer
    drawHud resources renderer hud
    SDL.present renderer
    
-- | Draws the head up display
drawHud :: Resources -> SDL.Renderer -> Hud -> IO ()
drawHud _ _ NoHud          = return ()
drawHud resources renderer (LevelHud polarity health) =
    let sprite = case polarity of
            Pink -> SpHeartPink
            Blue -> SpHeartBlue
        (sheet, source) = getSprite resources sprite
        dest pos = Just (Rectangle (P pos) (V2 30 30))
        doDraw pos = SDL.copy renderer (sheetTexture sheet) source (dest pos)
    in forM_ (map (fromIntegral . (\x -> 40 * x - 10)) [1..health]) $ \x ->
        doDraw (V2 x 10)
