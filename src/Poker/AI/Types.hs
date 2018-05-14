-- | All AI related types are declared here.
module Poker.AI.Types where

import System.Random (StdGen)

import Poker.Logic.Types.Cards

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

-- | Additional player data for AI needs.
data AIPlayer = AIPlayer
  { cards         :: [Card]    -- ^ dealt cards, including board
  , playStyle     :: PlayStyle -- ^ type of play style
  , madePFR       :: Bool      -- ^ made preflop raise
  , madeCbet      :: Bool      -- ^ made cbet on flop
  , madeSndBarrel :: Bool      -- ^ made second barrel on turn
  , madeTrdBarrel :: Bool      -- ^ made third  barrel on river
  , rng           :: StdGen    -- ^ random number generator
  }

-- | Data about game patterns.
data PlayStyle = PlayStyle
  { playStyleType     :: PlayStyleType    -- ^ name of playstyle
  , betSizeRangePF    :: BetRange         -- ^ in big blinds
  , pfHandPower       :: HandRangePF      -- ^ range of preflop hand
  , betRangePF        :: BetRange         -- ^ in % relative to hand power
  , callRangePF       :: BetRange         -- ^ in % relative to hand power
  , raiseRangePF      :: BetRange         -- ^ in % relative to hand power
  , cbet              :: Int              -- ^ % to cbet flop
  , sndBarrel         :: Int              -- ^ % to barrel turn  after cbet
  , thdBarrel         :: Int              -- ^ % to barrel river after turn
  , betSizeRangePostF :: BetRange         -- ^ in % of pot
  , handPower         :: CombinationRange -- ^ range of hand power
  , betRangePostF     :: BetRange         -- ^ in % relative to hand power
  , callRangePostF    :: BetRange         -- ^ in % relative to hand power
  , raiseRangePostF   :: BetRange         -- ^ in % relative to hand power
  , betSizings        :: BetSizings       -- ^ sizings for bets in %
  }

-- | Play style types.
data PlayStyleType
  = Telephone  -- ^ opens very huge range, raises almost never
  | Passive    -- ^ opens      huge range, raises occasionally
  | Loose      -- ^ opens       big range, raises periodically 
  | Tight      -- ^ opens     small range, raises often
  | Aggresive  -- ^ opens    medium range, raises very often 
  | Random     -- ^ choose one of previous five lines to act

-- | Container to hold range of hands.
data CardRange = CardRange
  { suitedRange    :: [(CardRank, CardRank)]
  , pairedRange    :: [CardRank]
  , offsuitedRange :: [(CardRank, CardRank)]
  }

-- | Type of bets.
data BetType
  = Small_Bet
  | Medium_Bet
  | Big_Bet
  | Huge_Bet
  deriving (Eq)

-- | Container to hold range of bets.
data BetRange = BetRange
  { smallBet  :: Int
  , mediumBet :: Int
  , bigBet    :: Int
  , hugeBet   :: Int
  }

-- | Type of hand power.
data HandPower
  = Weak_Hand
  | Medium_Hand
  | Strong_Hand
  | Monster_Hand

-- | Container to hold ranges of hands.
data HandRangePF = HandRangePF
  { vpipRange  :: CardRange
  , pfrRange   :: CardRange
  , raiseRange :: CardRange
  , pushRange  :: CardRange
  }

-- | Container to hold range of combinations.
data CombinationRange = CombinationRange
  { weakHand    :: Combination
  , mediumHand  :: Combination
  , strongHand  :: Combination
  , monsterHand :: Combination
  }

-- | Container to hold bet sizings.
data BetSizings = BetSizings
  { raisePF         :: Int
  , cbetFlop        :: Int
  , betPostF        :: Int
  , betDistribution :: Int
  , raisePostF      :: Int
  }
