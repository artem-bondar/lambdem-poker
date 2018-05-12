-- | All poker related types are declared here.
module Poker.Logic.Types where

import Data.List

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

---------------------------------------
-- ** Overall game types
---------------------------------------

-- | Possible game states. 
data GameState
  = Dealing_Hand
  | Posting_Blinds
  | Bet_Round
  | Show_Click
  | Waiting_User_Input
  | AI_Thinking
  | Next_Move
  | Start_Round
  | Finish_Hand
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
  }

-- | Types of players.
data PlayerType
  = Human
  | AI

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

---------------------------------------
-- ** Types related to playing cards
---------------------------------------

-- | Card deck.
data Deck = Deck
  { size  :: Int
  , body :: [Card]
  }

-- | Derive 'Show' class for 'Deck'.
instance Show Deck where
  show deck = "Deck Contain " ++ show (size deck) ++ " cards:\n" ++ insides
    where
      insides = intercalate " " (zipWith 
        (\x index -> (show index ++ ". " ++ show x ++ "\n"))
        (body deck) [1 :: Int .. 52])

-- | Hand ranks.
data HandRank
  = High_card
  | One_pair
  | Two_pair
  | Three_of_a_kind
  | Straight
  | Flush
  | Full_house
  | Four_of_a_kind
  | Straight_flush
  | Royal_flush
  deriving (Eq, Ord)

-- | Derive 'Show' class for 'HandRank'.
instance Show HandRank where
  show rank = case rank of
    High_card       -> "High card"
    One_pair        -> "Pair"
    Two_pair        -> "Two pair"
    Three_of_a_kind -> "Three of a kind"
    Straight        -> "Straight"
    Flush           -> "Flush"
    Full_house      -> "Full house"
    Four_of_a_kind  -> "Four of a kind"
    Straight_flush  -> "Straigh flush"
    Royal_flush     -> "Royal flush"

-- | Poker card.
data Card = Card
  { cardRank :: CardRank
  , suit     :: Suit
  }
  deriving (Eq)

-- | Derive 'Show' class for 'Card'.
instance Show Card where
  show card = show (cardRank card) ++ " of " ++ show (suit card) 

-- | Derive 'Enum' class for 'Card'.
instance Enum Card where
  toEnum index = Card
    { cardRank = toEnum $ index `div` 4
    , suit     = toEnum $ index `mod` 4
    }
  fromEnum card = fromEnum (cardRank card) * 4 + fromEnum (suit card)

-- | Card suites.
data Suit
  = Diamonds -- ^ ♦
  | Clubs    -- ^ ♣
  | Hearts   -- ^ ♥
  | Spades   -- ^ ♠
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | Card ranks.
data CardRank
  = Deuce -- ^ 2
  | Three -- ^ 3
  | Four  -- ^ 4
  | Five  -- ^ 5
  | Six   -- ^ 6
  | Seven -- ^ 7
  | Eight -- ^ 8
  | Nine  -- ^ 9
  | Ten   -- ^ T
  | Jack  -- ^ J
  | Queen -- ^ Q
  | King  -- ^ K
  | Ace   -- ^ A
  deriving (Eq, Ord, Bounded, Enum, Show)

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Contain all 52 cards.
createDeck :: Deck
createDeck = Deck
  { size = 52
  , body = Card <$> allCardRanks <*> allSuites 
  }

-- | List of all card short names.
allCardNames :: [String]
allCardNames = map cardToShortName (body createDeck)

-- | Convert card to short name.
cardToShortName :: Card -> String
cardToShortName card =
  rank ++ cardSuit
  where
    rank = case cardRank card of
      Ten   -> "T"
      Jack  -> "J"
      Queen -> "Q"
      King  -> "K"
      Ace   -> "A"
      _     -> show $ fromEnum (cardRank card) + 2
    cardSuit = case suit card of
      Diamonds -> "d"
      Clubs    -> "c"
      Hearts   -> "h"
      Spades   -> "s"

-- | Return list with all action names.
allActionNames :: [String]
allActionNames = map
  (show . (toEnum :: Int -> ActionType)) [0..4] ++ ["All-In"]

-- | List of all suites.
allSuites :: [Suit]
allSuites = [minBound..maxBound]

-- | List of all card ranks.
allCardRanks :: [CardRank]
allCardRanks = [minBound..maxBound]
