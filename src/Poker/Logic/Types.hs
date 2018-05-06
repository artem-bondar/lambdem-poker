-- | All poker related types are declared here.
module Poker.Logic.Types where

import Data.List

-- | Contains all personal player data.
data Player = Player
  { name     :: String
  , balance  :: Int
  , position :: Position
  , hand     :: Maybe (Card, Card)
  }

-- | Possible game states. 
data GameState
  = Dealing_Hand
  | Waiting_User_Input
  | AI_Thinking
  deriving (Eq)

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
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | Card deck.
data Deck = Deck
  { size  :: Int
  , cards :: [Card]
  }

-- | Derive 'Show' class for 'Deck'.
instance Show Deck where
  show deck = "Deck contains " ++ show (size deck) ++ " cards:\n" ++ insides
    where
      insides = intercalate " " (zipWith 
        (\x index -> (show index ++ ". " ++ show x ++ "\n"))
        (cards deck) [1 :: Int .. 52])

-- | Contains all 52 cards.
createDeck :: Deck
createDeck = Deck
  { size = 52
  , cards = Card <$> allCardRanks <*> allSuites 
  }

-- | List of all card short names.
allCardNames :: [String]
allCardNames = map cardToShorName (cards createDeck)

-- | Convert card to short name.
cardToShorName :: Card -> String
cardToShorName card = rank ++ cardSuit
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

-- | List of all suites.
allSuites :: [Suit]
allSuites = [minBound..maxBound]

-- | List of all card ranks.
allCardRanks :: [CardRank]
allCardRanks = [minBound..maxBound]

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
  deriving (Eq, Ord)

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
