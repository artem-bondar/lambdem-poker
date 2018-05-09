-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture
import System.Random (StdGen)

import Poker.Logic.Types

-- | Contain all data relative to table game screen.
data TableScreen = TableScreen
  { state      :: GameState   -- ^ current game state
  , timer      :: Float       -- ^ for detecting time  
  , players    :: [Player]    -- ^ info about every player
  , street     :: Street      -- ^ current street
  , handCount  :: Int         -- ^ current hand number
  , dealer     :: Seat        -- ^ position of dealer
  , blindSize  :: Int         -- ^ size of big blind
  , board      :: [Card]      -- ^ cards on board
  , randomizer :: StdGen      -- ^ random number generator
  , deck       :: Deck        -- ^ cards to deal
  , images     :: TableImages -- ^ images relative to screen
  }

-- | Contain all images relative to table game screen.
data TableImages = TableImages
  { background       :: Picture
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
  , betSiseText :: Picture
  }

-- | Value of all small buttons.
allBetSizings :: [Int]
allBetSizings = [40, 60, 80, 100]

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

-- | Value of all chips.
allChipValues :: [Int]
allChipValues = [1000, 500, 100, 25, 5, 1]
