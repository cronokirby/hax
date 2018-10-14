{-# LANGUAGE OverloadedStrings #-}
module Hax
    ( run
    )
where

import Apecs (runWith)
import Control.Monad (forM, unless)
import Data.Word (Word32)
import Linear (V2(..))
import qualified SDL

import Drawing (draw)
import Game.Input (Input, initialInput, gatherInput)
import Game.World
import Resources.Sprite (SpriteData, loadProjectSprites)


-- | The window configuration for the game
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
    { SDL.windowInitialSize = round <$> V2 worldWidth worldHeight
    }


-- | The main entry point for the game
run :: IO ()
run = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Hax" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Initialising data
    spriteData <- loadProjectSprites renderer
    world <- initWorld 
    runWith world initialiseGame
    -- Prepping rendering
    SDL.showWindow window
    -- Looping
    ticks <- SDL.ticks
    mainLoop renderer ticks spriteData initialInput world
    -- cleanup
    SDL.destroyWindow window
    SDL.quit


-- | The main loop of the game
mainLoop :: SDL.Renderer -> Word32 -> SpriteData -> Input -> World -> IO ()
mainLoop renderer ticks sprites input world = do
    events <- SDL.pollEvents
    toggleCheck <- SDL.getKeyboardState
    newTicks <- SDL.ticks
    let quit = any shouldQuit events
        newInput = gatherInput toggleCheck input
        dT = fromIntegral (newTicks - ticks) / 1000
    toDraw <- runWith world (stepGame dT newInput)
    draw toDraw sprites renderer
    unless quit (mainLoop renderer newTicks sprites newInput world)


-- | This is true of the window needs to close, or esc is prssed
shouldQuit :: SDL.Event -> Bool
shouldQuit event = case SDL.eventPayload event of
    SDL.KeyboardEvent kb ->
        SDL.keyboardEventKeyMotion kb == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kb) == SDL.KeycodeEscape
    SDL.QuitEvent -> True
    _ -> False
