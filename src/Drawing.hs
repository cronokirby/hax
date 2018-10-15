{-# LANGUAGE OverloadedStrings #-}
{-|
Description: Contains utility functions for drawing sprites
-}
module Drawing
    ( draw
    )
where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text, justifyRight, pack)
import Foreign.C.Types (CDouble(..), CInt)
import qualified SDL
import qualified SDL.Font
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))

import Game.Logic ( Position(..), Angle(..), Look(..), Polarity(..)
                  , Shape(..), LevelState(..))
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
draw :: (LevelState, [(Position, Maybe Angle, Look)]) -> Resources -> SDL.Renderer -> IO ()
draw (hud, toDraw) resources renderer = do
    clearScreen renderer
    forM_ toDraw $ \(pos, angle, look) ->
        renderLook pos (fromMaybe (Angle 0) angle) look resources renderer
    drawHud resources renderer hud
    SDL.present renderer
    
-- | Draws the head up display
drawHud :: Resources -> SDL.Renderer -> LevelState -> IO ()
drawHud _ _ NoLevel          = return ()
drawHud resources renderer (InLevel polarity health score) =
    let sprite = case polarity of
            Pink -> SpHeartPink
            Blue -> SpHeartBlue
        (sheet, source) = getSprite resources sprite
        dest pos = Just (Rectangle (P pos) (V2 30 30))
        doDraw pos = SDL.copy renderer (sheetTexture sheet) source (dest pos)
        textScore = 
            let correctedScore = min (max score 0) 999999999
            in justifyRight 9 '0' . pack . show $ correctedScore
    in do
        forM_ (map (fromIntegral . (\x -> 40 * x - 10)) 
            [1..health]) $ \x -> doDraw (V2 x 10)
        drawText textScore resources renderer
    
drawText :: Text -> Resources -> SDL.Renderer -> IO ()
drawText t (Resources _ font) renderer = do
    s <- SDL.Font.solid font (V4 0xFF 0xFF 0xFF 0xFF) t
    texture <- SDL.createTextureFromSurface renderer s
    let dest = Just (Rectangle (P (V2 390 5)) (V2 170 30))
    SDL.copy renderer texture Nothing dest
    return ()
