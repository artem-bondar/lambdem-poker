-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture
import System.Random (StdGen)

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

-- | Contain all data relative to table game screen.
data TableScreen = TableScreen
  { state      :: GameState   -- ^ current game state
  , timer      :: Float       -- ^ for detecting time  
  , players    :: [Player]    -- ^ info about every player
  , hero       :: String      -- ^ name of hero
  , street     :: Street      -- ^ current street
  , handCount  :: Int         -- ^ current hand number
  , dealer     :: Seat        -- ^ position of dealer
  , blindSize  :: Int         -- ^ size of big blind
  , sliderData :: Slider      -- ^ slider properties
  , board      :: [Card]      -- ^ cards on board
  , randomizer :: StdGen      -- ^ random number generator
  , deck       :: Deck        -- ^ cards to deal
  , images     :: TableImages -- ^ images relative to screen
  }

-- | Contain all images relative to table game screen.
data TableImages = TableImages
  { background       :: Picture
  , win              :: Picture
  , loss             :: Picture
  , table            :: Picture
  , seatBold         :: Picture
  , seatBoldActive   :: Picture
  , slider           :: Picture
  , sliderBall       :: Picture   
  , button           :: Picture
  , buttonClicked    :: Picture
  , buttonTexts      :: [ButtonText]
  , smallButton      :: Picture
  , smallButtonTexts :: [SmallButtonText]
  , betWindow        :: Picture  
  , deckLayout       :: DeckLayout
  , chipLayout       :: ChipLayout
  }

-- | Contains text to show on buttons.
data ButtonText = ButtonText
 { actionType :: ActionType
 , actionText :: Picture
 }

-- | Contains text to show on small buttons.
data SmallButtonText = SmallButtonText
  { betSizing   :: Int     -- ^ size of pot in percentage
  , betSizeText :: Picture
  }

-- | Contain layout for card deck.
data DeckLayout = DeckLayout
  { back  :: Picture   -- ^ image for back side of card
  , front :: [Picture] -- ^ images for front side of cards
  }

-- | Contain layout for chips.
data ChipLayout = ChipLayout
  { dealerChip :: Picture
  , stack      :: [Chip]
  }

-- | Contain chip size and image.
data Chip = Chip
  { value  :: Int
  , sprite :: Picture
  }

-- | Contain slider data.
data Slider = Slider
  { minValue     :: Int
  , maxValue     :: Int
  , currentValue :: Int
  , stepSize     :: Float -- ^ in pixels
  , ballPosition :: Float -- ^ in pixels
  , isSelected   :: Bool
  }

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Value of all chips.
allChipValues :: [Int]
allChipValues = [1000, 500, 100, 25, 5, 1]

-- | Value of all small buttons.
allBetSizings :: [Int]
allBetSizings = [40, 60, 80, 100]

-- | Height of button.
buttonHeight :: Float
buttonHeight = 56
