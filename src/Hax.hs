{-# LANGUAGE OverloadedStrings #-}
module Hax
    ( run
    )
where

import Apecs (runWith)
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word32)
import Linear (V2(..))
import qualified SDL
import qualified SDL.Font
import System.Directory (doesFileExist)
import qualified System.IO as IO

import Drawing (Rendering, runRendering, draw)
import Game.Input (Input, initialInput, gatherInput)
import Game.World
import Resources (Resources, loadProjectResources)


-- | The window configuration for the game
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
    { SDL.windowInitialSize = round <$> V2 worldWidth worldHeight
    }


-- | The main entry point for the game
run :: IO ()
run = do
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    window <- SDL.createWindow "Hax" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Initialising data
    resources <- loadProjectResources renderer
    world <- initWorld 
    scores <- readScores
    runWith world (makeNewGame scores)
    -- Prepping rendering
    SDL.showWindow window
    -- Looping
    ticks <- SDL.ticks
    runRendering renderer $ mainLoop ticks resources initialInput world
    -- cleanup
    SDL.destroyWindow window
    SDL.Font.quit
    SDL.quit


-- | The main loop of the game
mainLoop :: Word32 -> Resources -> Input -> World -> Rendering ()
mainLoop ticks resources input world = do
    events <- SDL.pollEvents
    toggleCheck <- SDL.getKeyboardState
    newTicks <- SDL.ticks
    let quit = any shouldQuit events
        newInput = gatherInput toggleCheck input
        dT = fromIntegral (newTicks - ticks) / 1000
    toDraw <- liftIO $ runWith world (stepGame dT newInput)
    draw toDraw resources dT
    if quit 
        then liftIO $ closeGame world
        else mainLoop newTicks resources newInput world


-- | This is true of the window needs to close, or esc is prssed
shouldQuit :: SDL.Event -> Bool
shouldQuit event = case SDL.eventPayload event of
    SDL.KeyboardEvent kb ->
        SDL.keyboardEventKeyMotion kb == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kb) == SDL.KeycodeEscape
    SDL.QuitEvent -> True
    _ -> False


-- TODO: Don't use a hardcoded file

-- | Closes the game and saves the scores
closeGame :: World -> IO ()
closeGame world = do
    scores <- runWith world exitInfo
    let textScores = map (T.pack . show) scores
        scoreString = T.intercalate "\n" textScores
    IO.withFile ".scores" IO.WriteMode $ \h ->
        T.hPutStr h scoreString


{- | Reads the scores into a list

This method will crash if the input file is bad in any
respect, but since we control what goes into the score file
this is fine.
-}
readScores :: IO [Int]
readScores = do
    exists <- doesFileExist ".scores"
    if exists
        then doread
        else return (take 10 $ repeat 0)
  where
    doread = IO.withFile ".scores" IO.ReadMode $ \h -> do
        txt <- T.hGetContents h
        let scores = map (read . T.unpack) (T.lines txt)
        return scores
