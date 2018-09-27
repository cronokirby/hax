{-# LANGUAGE OverloadedStrings #-}
module Hax
    ( run
    )
where

import Control.Monad (unless)
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))
import qualified SDL

import Resources.Load (SpriteData, loadProjectSprites, renderSprite)
import Resources.Sprite (SpriteIndex(..))


-- | The window configuration for the game
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
    { SDL.windowInitialSize = V2 800 800
    }


-- | The main entry point for the game
run :: IO ()
run = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Hax" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Initialising data
    spriteData <- loadProjectSprites renderer
    -- Prepping rendering
    SDL.showWindow window
    -- Looping
    mainLoop renderer spriteData
    -- cleanup
    SDL.destroyWindow window
    SDL.quit


-- | The main loop of the game
mainLoop :: SDL.Renderer -> SpriteData -> IO ()
mainLoop renderer sprites = do
    events <- SDL.pollEvents
    let quit = any isEscPressed events
    -- Set the window to black
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer
    let dest = Just $ Rectangle (P (V2 300 700)) (V2 30 30)
    renderSprite sprites SpSquare dest renderer
    SDL.present renderer
    unless quit (mainLoop renderer sprites)


-- | Checks whether or not esc is pressed inside of this event
isEscPressed :: SDL.Event -> Bool
isEscPressed event = case SDL.eventPayload event of
    SDL.KeyboardEvent kb ->
        SDL.keyboardEventKeyMotion kb == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kb) == SDL.KeycodeEscape
    _ -> False