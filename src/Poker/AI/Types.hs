-- | All AI related types are declared here.
module Poker.AI.Types where

import System.Random (StdGen)

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

data AIPlayer = AIPlayer
  { hand       :: [Card]    -- ^ dealt hand
  , board      :: [Card]    -- ^ current board
  , playStyle  :: PlayStyle -- ^ type of play style
  , pfr        :: Bool      -- ^ preflop raise
  , randomizer :: StdGen    -- ^ random number generator
  }

data PlayStyle = PlayStyle
  { playStyleType :: PlayStyleType

  }

data PlayStyleType
  = Telephone
  | Passive
  | Tight
  | Aggresive
  | Random

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

