module Poker.Logic.Types where

-- | Contains all personal player data.
data Player = Player
  { name     :: String
  , balance  :: Int
  , position :: Position
  }

-- | Poker positions
data Position = UTG1 | UTG2 | MP1 | MP2 | HJ | CO | BTN | SB | BB
