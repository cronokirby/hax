{-# LANGUAGE OverloadedStrings #-}
module Hax
    ( run
    )
where

import Apecs (runWith)
import Control.Monad (forM, unless)
import Data.Word (Word32)
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))
import qualified SDL

import Drawing (renderLook)
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
    mainLoop renderer ticks spriteData world
    -- cleanup
    SDL.destroyWindow window
    SDL.quit


-- | The main loop of the game
mainLoop :: SDL.Renderer -> Word32 -> SpriteData -> World -> IO ()
mainLoop renderer ticks sprites world = do
    events <- SDL.pollEvents
    toggleCheck <- SDL.getKeyboardState
    newTicks <- SDL.ticks
    let (quit, input) = handleKeys toggleCheck
        dT = fromIntegral (newTicks - ticks) / 1000
    toDraw <- runWith world (stepGame dT input)
    
    -- drawing
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    forM toDraw $ \(pos, look) ->
        renderLook pos look sprites renderer
    SDL.present renderer
    unless quit (mainLoop renderer newTicks sprites world)


handleKeys :: (SDL.Scancode -> Bool) -> (Bool, Maybe Direction)
handleKeys toggled = (quit, makeDir (map toggled keys))
  where
    keys = [SDL.ScancodeW, SDL.ScancodeS, SDL.ScancodeA, SDL.ScancodeD]
    makeDir [True, _, True, _] = Just (DUp ToggleLeft)
    makeDir [True, _, _, True] = Just (DUp ToggleRight)
    makeDir [True, _, _, _]    = Just (DUp ToggleStraight)
    makeDir [_, True, True, _] = Just (DDown ToggleLeft)
    makeDir [_, True, _, True] = Just (DDown ToggleRight)
    makeDir [_, True, _, _]    = Just (DDown ToggleStraight)
    makeDir [_, _, True, _]    = Just DLeft
    makeDir [_, _, _, True]    = Just DRight
    makeDir _                  = Nothing
    quit = toggled SDL.ScancodeEscape


-- | Checks whether or not esc is pressed inside of this event
isEscPressed :: SDL.Event -> Bool
isEscPressed event = case SDL.eventPayload event of
    SDL.KeyboardEvent kb ->
        SDL.keyboardEventKeyMotion kb == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kb) == SDL.KeycodeEscape
    _ -> False