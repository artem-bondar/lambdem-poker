-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture

import Poker.Logic.Types

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { state        :: GameState    -- ^ current game state  
  , totalPlayers :: Int          -- ^ amount of current players
  , playersData  :: [Player]     -- ^ info about every player
  , handCount    :: Int          -- ^ current hand number
  , bank         :: Maybe Int    -- ^ bank size
  , sideBank     :: Maybe Int    -- ^ side bank size
  , flop         :: Maybe [Card] -- ^ flop cards
  , turn         :: Maybe Card   -- ^ turn card
  , river        :: Maybe Card   -- ^ river card
  , images       :: TableImages  -- ^ images relative to screen
  }

-- | Possible game states. 
data GameState
  = Dealing_Hand
  | Waiting_User_Input
  | AI_Thinking

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
