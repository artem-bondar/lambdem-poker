-- | All types related to game process are declared here.
module Poker.Logic.Types.Game where

import Poker.AI.Types (AIPlayer(..))

import Poker.Logic.Types.Cards

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

---------------------------------------
-- ** Overall game types
---------------------------------------

-- | Possible game states. 
data GameState
  = Start_Hand
  | Dealing_Hand
  | Posting_Blinds
  | Start_Round
  | Bet_Round
  | Show_Click
  | Waiting_User_Input
  | AI_Thinking
  | Next_Move
  | Finish_Hand
  | Finish_Game
  deriving (Eq)

-- | Contain all personal player data.
data Player = Player
  { control  :: PlayerType
  , name     :: String
  , balance  :: Int
  , position :: Position
  , seat     :: Seat
  , hand     :: Maybe (Card, Card)
  , hideHand :: Bool
  , active   :: Bool
  , pressed  :: Int
  , move     :: Move
  , invested :: Int
  , aiData   :: Maybe AIPlayer
  }

-- | Types of players.
data PlayerType
  = Human
  | AI

---------------------------------------
-- ** Poker game types
---------------------------------------

-- | Poker positions.
data Position
  = UTG -- ^ Under The Gun
  | MP  -- ^ Middle Position
  | CO  -- ^ Cut-Off
  | BTN -- ^ Button
  | SB  -- ^ Small Blind
  | BB  -- ^ Big Blind
  deriving (Eq)

-- | Describes all seating positions.
data Seat
  = Bottom
  | Left_Down
  | Left_Up
  | Top
  | Right_Up
  | Right_Down

-- | Contain information about player move.
data Move = Move
  { action  :: MadeActionType
  , betSize :: Int
  }

-- | Types of action that player made.
data MadeActionType
  = Bankrupted
  | Waiting
  | Checked
  | Called
  | Folded
  | Raised
  | All_In_ed
  deriving (Eq)

-- | Types of possible action that player can make.
data ActionType
  = Fold
  | Check
  | Call
  | Bet
  | Raise
  | All_In
  deriving (Eq, Bounded, Enum, Show)

-- | Current hand progress.
data Street
  = Preflop
  | Flop
  | Turn
  | River
  | Showdown
  deriving (Eq, Enum)

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return list with all action names.
allActionNames :: [String]
allActionNames = map
  (show . (toEnum :: Int -> ActionType)) [0..4] ++ ["All-In"]
