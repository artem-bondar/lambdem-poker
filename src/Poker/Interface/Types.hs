-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture
import System.Random (StdGen)

import Poker.Logic.Types

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { state        :: GameState    -- ^ current game state
  , timer        :: Float        -- ^ for detecting time  
  , totalPlayers :: Int          -- ^ amount of current players
  , playersData  :: [Player]     -- ^ info about every player
  , handCount    :: Int          -- ^ current hand number
  , bank         :: Maybe Int    -- ^ bank size
  , sideBank     :: Maybe Int    -- ^ side bank size
  , flop         :: Maybe [Card] -- ^ flop cards
  , turn         :: Maybe Card   -- ^ turn card
  , river        :: Maybe Card   -- ^ river card
  , randomizer   :: StdGen       -- ^ random number generator
  , images       :: TableImages  -- ^ images relative to screen
  }

-- | Contains all images relative to table game screen.
data TableImages = TableImages
  { background :: Picture
  , table      :: Picture
  , seatBold   :: Picture
  , deckLayout :: DeckLayout
  }

-- | Contains layout for card deck.
data DeckLayout = DeckLayout
  { back :: Picture   -- ^ image for back side of card
  , deck :: [Picture] -- ^ images for all 52 cards
  }
