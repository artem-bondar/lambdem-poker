-- | All interface related types are declared here.
module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture

import Poker.Logic.Types

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { totalPlayers :: Int           -- ^ amount of current players
  , playersData  :: [Player]      -- ^ info about every player
  , handCount    :: Int           -- ^ current hand number
  , images       :: TableImages   -- ^ images relative to screen
  }

-- | Contains all images relative to table game screen.
data TableImages = TableImages
  { background :: Picture
  , table      :: Picture
  , deckLayout :: DeckLayout
  }

-- | Contains layout for card deck.
data DeckLayout = DeckLayout
  { back :: Picture   -- ^ image for back side of card
  , deck :: [Picture] -- ^ images for all 52 cards
  }
