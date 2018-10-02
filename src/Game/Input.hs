{-|
Description: Contains utility types and functions for handling input.
-}
module Game.Input
    ( ToggleInput(..)
    , advanceToggle
    , getToggle
    , HeldInput
    , getHeld
    , LRDirection(..)
    , UDDirection(..)
    , Direction(..)
    , Input(..)
    , initialInput
    , gatherInput
    )
where

import Data.List (uncons)
import qualified SDL


-- | Describes an input that toggles some kind of actions
-- We want this to behave such that a held button doesn't trigger
-- firing more than once.
data ToggleInput = ToggleInput !Bool !Bool deriving Show

-- | Advanced the state of a held input based on the current frame
advanceToggle :: Bool -> ToggleInput -> ToggleInput
advanceToggle b (ToggleInput now lastFrame) =
    (ToggleInput (b && not lastFrame) b)

-- | The state of a held input at the start of a game
makeToggle :: ToggleInput
makeToggle = ToggleInput False False

getToggle :: ToggleInput -> Bool
getToggle (ToggleInput i _) = i


-- | Describes an input that keeps an action held
newtype HeldInput = HeldInput Bool

getHeld :: HeldInput -> Bool
getHeld (HeldInput b) = b


-- | Left / Right direction toggle
data LRDirection = DLeft | DRight

-- | Up / Right direction toggle
data UDDirection = DUp | DDown

-- | Represents directions
data Direction = Direction (Maybe LRDirection) (Maybe UDDirection)

checkDirection :: [Bool] -> [Bool] -> Direction
checkDirection leftRights upDowns = Direction
    (getToggle leftRights [DLeft, DRight])
    (getToggle upDowns [DUp, DDown])
  where
    getToggle :: [Bool] -> [a] -> Maybe a
    getToggle bools enums =
        fmap fst . uncons . map snd . filter fst $ zip bools enums


-- | Represents the full input at some point in the game
data Input = Input
    { inputShooting :: HeldInput -- ^ Whether or not we're shooting
    , inputSwitch :: ToggleInput -- ^ Trigger polarity switch
    , inputSlow :: HeldInput -- ^ Whether or not to move slower
    , inputDirection :: Direction -- ^ What direction is being input
    }

-- | The state of input before any interaction has been gathered
initialInput :: Input
initialInput = Input
    (HeldInput False)
    makeToggle
    (HeldInput False)
    (Direction Nothing Nothing)

-- | Gathers input based on what scancodes are currently activated
-- Requires the previous input state to be able to advance the state
-- of input gathering.
gatherInput :: (SDL.Scancode -> Bool) -> Input -> Input
gatherInput toggle (Input _ oldSwitch _ _) =
    Input shooting switch fast direction
  where
    shooting = HeldInput (toggle SDL.ScancodeJ)
    switch = advanceToggle (toggle SDL.ScancodeK) oldSwitch
    fast = HeldInput (toggle SDL.ScancodeL)
    leftRights = map toggle [SDL.ScancodeA, SDL.ScancodeD]
    upDowns = map toggle [SDL.ScancodeW, SDL.ScancodeS]
    direction = checkDirection leftRights upDowns