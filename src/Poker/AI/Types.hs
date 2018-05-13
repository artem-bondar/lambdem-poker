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
  { playStyleType   :: PlayStyleType    -- ^ name of playstyle
  , betRangePF      :: BetRange         -- ^ in big blinds
  , callPFSmall     :: CardRange        -- ^ range to call on preflop
  , callPFMedium    :: CardRange        -- ^ range to call on preflop
  , callPFBig       :: CardRange        -- ^ range to call on preflop
  , raisePFRange    :: CardRange        -- ^ range to raise on preflop
  , pushPFRange     :: CardRange        -- ^ range to push  on preflop 
  , cbet            :: Int              -- ^ % to cbet flop
  , handPower       :: CombinationRange -- ^ range of hand power
  , betRangePostF   :: BetRange         -- ^ in % relative to hand power
  , callRangePostF  :: BetRange         -- ^ in % relative to hand power
  , raiseRangePostF :: BetRange         -- ^ in % relative to hand power
  , betSizings      :: BetSizings       -- ^ sizings for bets
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

-- | Container to hold range of combinations.
data CombinationRange = CombinationRange
  { weakHand    :: Combination
  , mediumHand  :: Combination
  , strongHand  :: Combination
  , monsterHand :: Combination
  }

-- | Container to hold bet sizings.
data BetSizings = BetSizings
  { raisePF    :: Int
  , cbetFlop   :: Int
  , betPostF   :: Int
  , raisePostF :: Int
  }

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

