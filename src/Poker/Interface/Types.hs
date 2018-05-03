module Poker.Interface.Types where

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { totalPlayers :: Int           -- amount of current players
  , playersData  :: [Player]      -- info about every player
  , handCount    :: Int           -- current hand number
  , screenImages :: Images   -- all images
  }

-- | All images relative to table game screen.
data Images = Images
 { background :: Picture
 , table      :: Picture
 }
 