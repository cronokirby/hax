{-|
Description: Contains abstract data related to Sprites

This module also acts as a global source of truth with regards to
what sprites can exist. As such, it exports Sprite indices which
are always valid, and a mapping to file resources which must always
be kept in sync with the actual resources folder used in development.
-}
module Resources
    ( SpriteIndex(..)
    , SpriteSheet(..)
    , sheetTexture
    , Resources(..)
    , getSprite
    , loadProjectResources
    )
where

import Data.Function ((&))
import Foreign.C.Types (CInt)
import System.IO (FilePath)
import qualified SDL
import qualified SDL.Font
import SDL (Rectangle(..), Point(..), V2(..))


{- | Contains the necessary information to locate a Sprite's sheet
CInt is mainly used for conveniance of interaction with SDL
-}
data SpriteLocation = SpriteLocation !CInt !CInt !FilePath

{- | The locations for sprites used in this project.

This should always be kept in sync with the /resources folder.
-}
spriteLocations :: [SpriteLocation]
spriteLocations = map makeLocation
    [ (60, 60, "square.bmp")
    , (60, 60, "heart.bmp")
    ]
  where
    spritePath = "resources/sprites/"
    makeLocation (w, h, path) = SpriteLocation w h (spritePath ++ path)


{- | The global index of all possible sprites and animations.

The tag indicates the spritesheet, and the second the element.
-}
data SpriteIndex
    = SpSquarePink | SpSquareBlue
    | SpHeartPink | SpHeartBlue

-- | Contains a raw index in (spritesheet, spriteframe) form
data RawIndex = RawIndex !Int !Int

-- | Maps sprite indices to a raw index
getRaw :: SpriteIndex -> RawIndex
getRaw SpSquarePink   = RawIndex 0 0
getRaw SpSquareBlue   = RawIndex 0 1
getRaw SpHeartPink    = RawIndex 1 0
getRaw SpHeartBlue    = RawIndex 1 1


{- | Used to contain the data for a Sprite and all its animations.

Each sprite in the sheet must have the same size in Width Height.
-}
data SpriteSheet = SpriteSheet !CInt !CInt !SDL.Texture

sheetTexture :: SpriteSheet -> SDL.Texture
sheetTexture (SpriteSheet _ _ texture) = texture

-- | Returns the source rectangle given the index of a spritesheet
getFrame :: SpriteSheet -> Int -> Maybe (Rectangle CInt)
getFrame (SpriteSheet w h texture) index = Just $
    Rectangle (P (V2 (w * fromIntegral index) 0)) (V2 w h)


-- | Wrapper around the filepath for a ttf font
newtype FontLocation = FontLocation FilePath

-- | Global font location reference, in sync with filesystem
fontLocation :: FontLocation
fontLocation = FontLocation "resources/fonts/joystix.ttf"

-- | Contains all the resource information for the game
data Resources = Resources 
    { spriteData :: [SpriteSheet]
    , font :: SDL.Font.Font
    }

{- | Gets the sprite sheet and frame corresponding to a given index

This allows us to keep SpriteData's representation independent
-}
getSprite :: Resources -> SpriteIndex -> (SpriteSheet, Maybe (Rectangle CInt))
getSprite (Resources sheets _) index =
    let (RawIndex sheetPos frame) = getRaw index
        sheet = sheets !! sheetPos
    in (sheet, getFrame sheet frame)


-- | Loads sprites from a list of locations into a renderer
loadResources :: SDL.Renderer 
              -> [SpriteLocation] -> FontLocation
              -> IO Resources
loadResources renderer spriteLs fontL = do
    sprites <- mapM loadSprite spriteLs
    font <- loadFont fontL
    return (Resources sprites font)
  where
    loadSprite (SpriteLocation w h path) =
        SDL.loadBMP path >>=
        SDL.createTextureFromSurface renderer &
        fmap (SpriteSheet w h)
    loadFont (FontLocation path) = 
            SDL.Font.load path 20

-- | Loads the sprites using the locations in Resources.Sprite
loadProjectResources :: SDL.Renderer -> IO Resources
loadProjectResources r =
    loadResources r spriteLocations fontLocation


-- | Renders a sprite from sprite data, given an index
renderSprite :: Resources -> SpriteIndex -> Maybe (Rectangle CInt)
             -> SDL.Renderer -> IO ()
renderSprite (Resources sheets _) index dest r =
    let (RawIndex sheetPos frame) = getRaw index
        -- Always in the list, because we're keeping locations in sync
        sheet = sheets !! sheetPos
        source = getFrame sheet frame
    in SDL.copy r (sheetTexture sheet) source dest
