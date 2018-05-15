-- | Contains constructors for playstyles.
module Poker.AI.PlayStyles where

import System.Random (StdGen)

import Poker.AI.Ranges
import Poker.AI.Types

import Poker.Logic.Types.Cards

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

-- | Return new AI player data with defined playstyle and generator.
getAIPlayer :: PlayStyleType -> StdGen -> AIPlayer
getAIPlayer Telephone randomizer = AIPlayer [] getTelephonePlaystyle False False False randomizer

-- | Telephone.
getTelephonePlaystyle :: PlayStyle
getTelephonePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 8
      , mediumBet = 12 
      , bigBet    = 16
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = seventyOpen
      , pfrRange   = bestPremium
      , raiseRange = bestPremium
      , pushRange  = highPremium
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
    , cbet      = 0
    , sndBarrel = 0
    , trdBarrel = 0
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 100
      , mediumBet = 150
      , bigBet    = 200
      , hugeBet   = 0
      }
  , handPower = CombinationRange
      { weakHand    = Combination One_pair [Deuce] [Deuce, Deuce, Deuce]
      , mediumHand  = Combination Two_pair [Deuce, Deuce] [Deuce]
      , strongHand  = Combination Three_of_a_kind [Deuce] [Deuce, Deuce]
      , monsterHand = Combination Straight [Deuce] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 5
      , mediumBet = 10
      , bigBet    = 25
      , hugeBet   = 35
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 90
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

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
