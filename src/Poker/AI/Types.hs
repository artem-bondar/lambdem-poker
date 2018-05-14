-- | All AI related types are declared here.
module Poker.AI.Types where

import System.Random (StdGen)

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Declarations
-------------------------------------------------------------------------------

-- | Additional player data for AI needs.
data AIPlayer = AIPlayer
  { cards      :: [Card]    -- ^ dealt cards, including board
  , suited     :: Bool      -- ^ if hand is suited
  , paired     :: Bool      -- ^ if hand is paired
  , playStyle  :: PlayStyle -- ^ type of play style
  , pfr        :: Bool      -- ^ preflop raise
  , rng        :: StdGen    -- ^ random number generator
  }

-- | Data about game patterns.
data PlayStyle = PlayStyle
  { playStyleType     :: PlayStyleType    -- ^ name of playstyle
  , betSizeRangePF    :: BetRange         -- ^ in big blinds
  , PFHandPower       :: HandRangePF      -- ^ range of preflop hand 
  , betRangePF        :: BetRange         -- ^ in % relative to hand power
  , callRangePF       :: BetRange         -- ^ in % relative to hand power
  , raiseRangePF      :: BetRange         -- ^ in % relative to hand power
  , cbet              :: Int              -- ^ % to cbet flop
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
  | Huge_bet

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
  { vpipRange :: CardRange
  , pfrRange  :: CardRange
  , 3bRange   :: CardRange
  , pushRange :: CardRange
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

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

-- | Telephone.
getTelephonePlaystyle :: PlayStyle
getTelephonePlaystyle =
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 4
      , mediumBet = 8 
      , bigBet    = 12
      , hugeBet   = 20
      }
  , PFHandPower = HandRangePF
      { vpipRange = seventyOpen
      , pfrRange  = bestPremium
      , 3bRange   = bestPremium
      , pushRange = highPremium
      }
  , betRangePF = BetRange -- in %
      { smallBet  = 100
      , mediumBet = 100 
      , bigBet    = 100
      , hugeBet   = 100
      }
    , callRangePF = BetRange -- in %
      { smallBet  = 90
      , mediumBet = 100
      , bigBet    = 100
      , hugeBet   = 100
      }
    , raiseRangePF = BetRange -- in %
      { smallBet  = 1
      , mediumBet = 90
      , bigBet    = 80
      , hugeBet   = 70
      }
    , cbet = 0
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 30
      , mediumBet = 60
      , bigBet    = 80
      , hugeBet   = 120
      }
  , handPower = CombinationRange
      { weakHand    = Combination Two_pair [Ace, King] []
      , mediumHand  = Combination Straight [Ace] []
      , strongHand  = Combination Flush [Ace] []
      , monsterHand = Combination Full_house [Ace, King] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 5
      , mediumBet = 15
      , bigBet    = 25
      , hugeBet   = 40
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 80
      , mediumBet = 95
      , bigBet    = 100
      , hugeBet   = 100
      }
    , raiseRangePostF = BetRange -- in %
      { smallBet  = 0
      , mediumBet = 3
      , bigBet    = 7
      , hugeBet   = 12
      }
    , betSizings = BetSizings
      { raisePF         = 200
      , cbetFlop        = 0
      , betPostF        = 40
      , betDistribution = 10
      , raisePostF      = 200
      }
  }
