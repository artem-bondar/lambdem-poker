module Poker.Interface.Types where

import Graphics.Gloss.Data.Picture

import Poker.Logic.Types

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { totalPlayers :: Int           -- amount of current players
  , playersData  :: [Player]      -- info about every player
  , handCount    :: Int           -- current hand number
  , images       :: TableImages   -- images relative to screen
  }
 
-- | All images relative to table game screen.
data TableImages = Images
  { background :: Picture
  , table      :: Picture
  }
