-- | All poker related types are declared here.
module Poker.Logic.Types where

-- | Contains all personal player data.
data Player = Player
  { name     :: String
  , balance  :: Int
  , position :: Position
  }

-- | Poker positions.
data Position
  = UTG1 -- ^ Under The Gun
  | UTG2 -- ^ Under The Gun
  | MP1  -- ^ Middle Position
  | MP2  -- ^ Middle Position
  | HJ   -- ^ High Jack
  | CO   -- ^ Cut-Off
  | BTN  -- ^ Button
  | SB   -- ^ Small Blind
  | BB   -- ^ Big Blind
