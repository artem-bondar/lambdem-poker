-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture
import System.Random (StdGen)

import Poker.Logic.Types

-- | Contain all data relative to table game screen.
data TableScreen = TableScreen
  { state      :: GameState    -- ^ current game state
  , timer      :: Float        -- ^ for detecting time  
  , players    :: [Player]     -- ^ info about every player
  , street     :: Street       -- ^ current street
  , acting     :: Position     -- ^ position of active player
  , maxBet     :: Int          -- ^ maximal bet that occured
  , retrade    :: Bool         -- ^ need to repeat bet round
  , handCount  :: Int          -- ^ current hand number
  , dealer     :: Seat         -- ^ position of dealer
  , blindSize  :: Int          -- ^ size of big blind
  , pot        :: Int          -- ^ pot size
  , sidePot    :: Int          -- ^ side pot size
  , flop       :: Maybe [Card] -- ^ flop cards
  , turn       :: Maybe Card   -- ^ turn card
  , river      :: Maybe Card   -- ^ river card
  , randomizer :: StdGen       -- ^ random number generator
  , deck       :: Deck         -- ^ cards to deal
  , images     :: TableImages  -- ^ images relative to screen
  }

-- | Contain all images relative to table game screen.
data TableImages = TableImages
  { background :: Picture
  , table      :: Picture
  , seatBold   :: Picture
  , deckLayout :: DeckLayout
  , chipLayout :: ChipLayout
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

-- | Value of all chips.
allChipValues :: [Int]
allChipValues = [1000, 500, 100, 25, 5, 1]
