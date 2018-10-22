{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-|
Description: Contains utility functions for drawing sprites
-}
module Drawing
    ( Rendering
    , runRendering
    , draw
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, put)
import Data.Maybe (fromMaybe)
import Data.Text (Text, justifyRight, pack)
import Data.Word (Word8)
import Foreign.C.Types (CDouble(..), CInt)
import qualified SDL
import qualified SDL.Font
import SDL (($=), Rectangle(..), Point(..), V2(..), V4(..))

import Game.Logic
import Resources


type Color = V4 Word8

white :: Color
white = V4 0xFF 0xFF 0xFF 0xFF

black :: Color
black = V4 0 0 0 0xFF


-- | Gets the sprite index corresponding to a given look
spriteIndex :: Look -> SpriteIndex
spriteIndex (Look _ SquareShape Pink)   = SpSquarePink
spriteIndex (Look _ SquareShape Blue)   = SpSquareBlue

-- | Gets the destination rectangle for a sprite, given a central position
getDestination :: Position -> Look -> SpriteSheet -> Maybe (Rectangle CInt)
getDestination (Position pos) (Look width _ _) (SpriteSheet w h texture) =
    let shift = V2 width (width / fromIntegral w * fromIntegral h)
        topLeft = pos - fmap (/ 2) shift
    in Just . fmap round $ Rectangle (P topLeft) shift



type ShakeTimeLine = TimeLine CInt

newtype Rendering a = Rendering 
    { getRendering :: ReaderT SDL.Renderer (StateT ShakeTimeLine IO) a
    }
    deriving (Functor, Applicative, Monad
             , MonadReader SDL.Renderer, MonadIO
             , MonadState ShakeTimeLine
             )

runRendering :: SDL.Renderer -> Rendering a -> IO a
runRendering renderer = 
    (`evalStateT` makeTimeLineOnce []) .
    (`runReaderT` renderer) . 
    getRendering


-- | Renders a Look to a position given data about sprites and a renderer
renderLook :: Position -> Angle -> Look -> Resources -> Rendering ()
renderLook position (Angle alpha) look resources =
    let index = spriteIndex look
        (sheet, source) = getSprite resources index
        dest = getDestination position look sheet
        toFlip = V2 False False
        angle = CDouble alpha
    in do
        renderer <- ask
        SDL.copyEx renderer (sheetTexture sheet) source dest angle Nothing toFlip

-- | Clears the background
clearScreen :: Rendering ()
clearScreen = do
    renderer <- ask
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= black


handleEffect :: ScreenEffect -> Rendering ()
handleEffect NoScreenEffect = return ()
handleEffect ScreenShake =
    let tl = makeTimeLineOnce (zip [0,0.002..] ([1..20] ++ [20,19..0]))
    in put tl

-- | Handles the shake timeline
handleShakeTimeLine :: Double -> Rendering ()
handleShakeTimeLine dT = do
    tl <- get
    let (newTl, offset) = stepTimeLine tl dT
    put newTl
    maybe (return ()) moveViewPort offset
  where
    moveViewPort :: CInt -> Rendering ()
    moveViewPort o =
        let dest = P (V2 o 0)
        in do
            renderer <- ask
            SDL.rendererViewport renderer $= Just (Rectangle dest (V2 600 800))
        


-- | Draws all the sprites, including the background
draw :: RenderInfo -> Resources -> Double -> Rendering ()
draw (RenderInfo hud toDraw effect) resources dT = do
    renderer <- ask
    clearScreen
    handleEffect effect
    handleShakeTimeLine dT
    forM_ toDraw $ \(pos, angle, look) ->
        renderLook pos (fromMaybe (Angle 0) angle) look resources
    drawHud resources hud
    SDL.present renderer
    
-- | Draws the head up display
drawHud :: Resources  -> LevelState -> Rendering ()
drawHud resources (GameOver select p) = 
    let destGameOver = Just (Rectangle (P (V2 180 250)) (V2 240 50))
        destContinue = Just (Rectangle (P (V2 220 370)) (V2 160 40))
        destTitle    = Just (Rectangle (P (V2 230 420)) (V2 140 36))
        baseLook = Look 28 SquareShape
        (pos, look@(Look _ _ polarity), colorC, colorT) = case select of
            GOContinue    -> ( V2 180 390, baseLook p
                             , polarityToColor p, white )
            GOTitleScreen -> ( V2 190 440, baseLook p
                             , white, polarityToColor p )
        
    in do
        drawText "game over" destGameOver white resources 
        drawText "continue" destContinue colorC resources
        drawText "give up" destTitle colorT resources
        renderLook (Position pos) (Angle 0) look resources
drawHud resources (TitleScreen select p) =
    let destTitle  = Just (Rectangle (P (V2 150 100)) (V2 300 200))
        destPlay   = Just (Rectangle (P (V2 230 400)) (V2 140 36))
        destScores = Just (Rectangle (P (V2 220 450)) (V2 160 40))
        baseLook = Look 28 SquareShape
        (pos, look@(Look _ _ polarity), colorP, colorS) = case select of
            TSPlay   -> ( V2 190 420, baseLook p
                        , polarityToColor p, white )
            TSScores -> ( V2 180 470, baseLook p
                        , white, polarityToColor p )
    in do
        drawText "HAX" destTitle white resources
        drawText "new game" destPlay colorP resources
        drawText "scoreboard" destScores colorS resources
        renderLook (Position pos) (Angle 0) look resources
drawHud resources (InLevel polarity health score) =
    let sprite = case polarity of
            Pink -> SpHeartPink
            Blue -> SpHeartBlue
        (sheet, source) = getSprite resources sprite
        dest pos = Just (Rectangle (P pos) (V2 30 30))
    in do
        forM_ (map (fromIntegral . (\x -> 40 * x - 10)) 
            [1..health]) $ \x -> do
                renderer <- ask
                SDL.copy renderer (sheetTexture sheet) source (dest (V2 x 10))
        drawScore score resources
    
drawScore :: Int -> Resources -> Rendering ()
drawScore score = drawText txt dest white
 where
    correctedScore = min (max score 0) 999999999
    txt = justifyRight 9 '0' . pack . show $ correctedScore
    dest = Just (Rectangle (P (V2 390 5)) (V2 170 30))

drawText :: Text -> Maybe (Rectangle CInt) -> V4 Word8 -> Resources -> Rendering ()
drawText txt dest color (Resources _ font) = do
    s <- SDL.Font.solid font color txt
    renderer <- ask
    texture <- SDL.createTextureFromSurface renderer s
    SDL.copy renderer texture Nothing dest
