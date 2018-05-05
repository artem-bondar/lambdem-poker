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
  deriving (Eq, Ord, Show)

-- | Poker combinations.
data Hand = Hand
  { handRank   :: HandRank
  , body       :: [Card]  -- ^ from 5 to 7 cards
  , kicker     :: [Card]  -- ^ kicker cards, amount
                          --   depends on hand rank
  }

-- | Poker card.
data Card = Card
  { cardRank :: CardRank
  , suit     :: Suit
  }

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
  deriving (Eq, Ord, Bounded, Enum)

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
