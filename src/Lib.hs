{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    )
where

import Control.Monad (unless)
import SDL (($=), Rectangle(..))
import qualified SDL
import SDL.Vect (Point(..), V2(..), V4(..))


-- | The window configuration for the game
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
    { SDL.windowInitialSize = V2 600 800
    }


-- | The main entry point for the game
run :: IO ()
run = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Hax" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.showWindow window
    surface <- SDL.loadBMP "resources/sprites/square.bmp"
    texture <- SDL.createTextureFromSurface renderer surface
    mainLoop renderer texture
    SDL.destroyWindow window
    SDL.quit


-- | The main loop of the game
mainLoop :: SDL.Renderer -> SDL.Texture -> IO ()
mainLoop renderer texture = do
    events <- SDL.pollEvents
    let quit = any isEscPressed events
    -- Set the window to black
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer
    let dest = Just $ Rectangle (P (V2 300 400)) (V2 30 30)
    SDL.copy renderer texture Nothing dest
    SDL.present renderer
    unless quit (mainLoop renderer texture)


-- | Checks whether or not esc is pressed inside of this event
isEscPressed :: SDL.Event -> Bool
isEscPressed event = case SDL.eventPayload event of
    SDL.KeyboardEvent kb ->
        SDL.keyboardEventKeyMotion kb == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kb) == SDL.KeycodeEscape
    _ -> False