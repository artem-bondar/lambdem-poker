-- | Contains constructors for playstyles.
module Poker.AI.PlayStyles where

import Poker.AI.Types

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

-- | Telephone.
getTelephonePlaystyle :: PlayStyle
getTelephonePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 4
      , mediumBet = 8 
      , bigBet    = 12
      , hugeBet   = 20
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

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------
