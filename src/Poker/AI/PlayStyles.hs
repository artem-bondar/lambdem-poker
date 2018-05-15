-- | Contains constructors for playstyles.
module Poker.AI.PlayStyles where

import System.Random (StdGen, randomR)

import Poker.AI.Ranges
import Poker.AI.Types

import Poker.Logic.Types.Cards

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return new AI player data with defined playstyle and generator.
getAIPlayer :: PlayStyleType -> StdGen -> AIPlayer
getAIPlayer playstyle randomizer = case playstyle of
  Telephone -> AIPlayer [] telephonePlaystyle           False False False randomizer
  Passive   -> AIPlayer [] passivePlaystyle             False False False randomizer
  Loose     -> AIPlayer [] loosePlaystyle               False False False randomizer
  Tight     -> AIPlayer [] tightPlaystyle               False False False randomizer
  Aggresive -> AIPlayer [] aggresivePlaystyle           False False False randomizer
  Random    -> AIPlayer [] (randomPlaystyle randomizer) False False False randomizer

randomPlaystyle :: StdGen -> PlayStyle
randomPlaystyle randomizer = case toEnum $ fst randomResult of
  Telephone -> telephonePlaystyle
  Passive   -> passivePlaystyle
  Loose     -> loosePlaystyle
  Tight     -> tightPlaystyle
  _         -> aggresivePlaystyle
  where
    randomResult = randomR (0, 4) randomizer

-------------------------------------------------------------------------------
-- * Constructors
-------------------------------------------------------------------------------

-- | Telephone.
telephonePlaystyle :: PlayStyle
telephonePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 8
      , mediumBet = 12 
      , bigBet    = 16
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = eightyFiveOpen
      , pfrRange   = bestPremium
      , raiseRange = bestPremium
      , pushRange  = highPremium
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

-- | Passive.
passivePlaystyle :: PlayStyle
passivePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 6
      , mediumBet = 10 
      , bigBet    = 16
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = seventyOpen
      , pfrRange   = highPremium
      , raiseRange = highPremium
      , pushRange  = highPremium
      }
    , cbet      = 0
    , sndBarrel = 0
    , trdBarrel = 0
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 85
      , mediumBet = 135
      , bigBet    = 170
      , hugeBet   = 0
      }
  , handPower = CombinationRange
      { weakHand    = Combination One_pair [Eight] [Deuce, Deuce, Deuce]
      , mediumHand  = Combination Two_pair [Deuce, Deuce] [Deuce]
      , strongHand  = Combination Straight [Deuce] [Deuce, Deuce]
      , monsterHand = Combination Flush [Deuce] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 12
      , mediumBet = 15
      , bigBet    = 25
      , hugeBet   = 40
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 100
      , mediumBet = 100
      , bigBet    = 100
      , hugeBet   = 100
      }
    , raiseRangePostF = BetRange -- in %
      { smallBet  = 0
      , mediumBet = 15
      , bigBet    = 20
      , hugeBet   = 35
      }
    , betSizings = BetSizings
      { raisePF         = 200
      , cbetFlop        = 0
      , betPostF        = 50
      , betDistribution = 15
      , raisePostF      = 220
      }
  }

-- | Loose.
loosePlaystyle :: PlayStyle
loosePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 6
      , mediumBet = 9 
      , bigBet    = 14
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = sixtyOpen
      , pfrRange   = mediumPremium
      , raiseRange = highPremium
      , pushRange  = highPremium
      }
    , cbet      = 35
    , sndBarrel = 45
    , trdBarrel = 20
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 65
      , mediumBet = 100
      , bigBet    = 175
      , hugeBet   = 0
      }
  , handPower = CombinationRange
      { weakHand    = Combination One_pair [Deuce] [Deuce, Deuce, Deuce]
      , mediumHand  = Combination Two_pair [Deuce, Deuce] [Deuce]
      , strongHand  = Combination Three_of_a_kind [Deuce] [Deuce, Deuce]
      , monsterHand = Combination Straight [Deuce] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 25
      , mediumBet = 35
      , bigBet    = 45
      , hugeBet   = 60
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 60
      , mediumBet = 80
      , bigBet    = 95
      , hugeBet   = 100
      }
    , raiseRangePostF = BetRange -- in %
      { smallBet  = 5
      , mediumBet = 10
      , bigBet    = 20
      , hugeBet   = 40
      }
    , betSizings = BetSizings
      { raisePF         = 240
      , cbetFlop        = 65
      , betPostF        = 60
      , betDistribution = 15
      , raisePostF      = 265
      }
  }

-- | Tight.
tightPlaystyle :: PlayStyle
tightPlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 6
      , mediumBet = 9 
      , bigBet    = 14
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = btnPFR
      , pfrRange   = broadwayToFives
      , raiseRange = mediumPremium
      , pushRange  = highPremium
      }
    , cbet      = 65
    , sndBarrel = 50
    , trdBarrel = 35
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 65
      , mediumBet = 100
      , bigBet    = 150
      , hugeBet   = 0
      }
  , handPower = CombinationRange
      { weakHand    = Combination One_pair [Eight] [Deuce, Deuce, Deuce]
      , mediumHand  = Combination Two_pair [Deuce, Deuce] [Deuce]
      , strongHand  = Combination Straight [Deuce] [Deuce, Deuce]
      , monsterHand = Combination Flush [Deuce] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 55
      , mediumBet = 65
      , bigBet    = 75
      , hugeBet   = 90
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 50
      , mediumBet = 85
      , bigBet    = 90
      , hugeBet   = 100
      }
    , raiseRangePostF = BetRange -- in %
      { smallBet  = 10
      , mediumBet = 20
      , bigBet    = 30
      , hugeBet   = 40
      }
    , betSizings = BetSizings
      { raisePF         = 270
      , cbetFlop        = 70
      , betPostF        = 65
      , betDistribution = 15
      , raisePostF      = 285
      }
  }

-- | Aggresive.
aggresivePlaystyle :: PlayStyle
aggresivePlaystyle = PlayStyle
  { playStyleType  = Telephone
  , betSizeRangePF = BetRange -- in big blinds
      { smallBet  = 6
      , mediumBet = 9 
      , bigBet    = 14
      , hugeBet   = 0
      }
  , pfHandPower = HandRangePF
      { vpipRange  = seventyOpen
      , pfrRange   = coPFR
      , raiseRange = broadwayToFives
      , pushRange  = lowPremium
      }
    , cbet      = 85
    , sndBarrel = 75
    , trdBarrel = 65
    , betSizeRangePostF = BetRange -- in % of pot
      { smallBet  = 65
      , mediumBet = 100
      , bigBet    = 150
      , hugeBet   = 0
      }
  , handPower = CombinationRange
      { weakHand    = Combination High_card [Ace, Deuce, Deuce, Deuce, Deuce] []
      , mediumHand  = Combination One_pair [Nine] [Deuce]
      , strongHand  = Combination Two_pair [Deuce, Deuce] [Deuce]
      , monsterHand = Combination Three_of_a_kind [Deuce] []
      }
  , betRangePostF = BetRange -- in %
      { smallBet  = 50
      , mediumBet = 75
      , bigBet    = 85
      , hugeBet   = 95
      }
    , callRangePostF = BetRange -- in %
      { smallBet  = 40
      , mediumBet = 65
      , bigBet    = 90
      , hugeBet   = 100
      }
    , raiseRangePostF = BetRange -- in %
      { smallBet  = 30
      , mediumBet = 40
      , bigBet    = 65
      , hugeBet   = 90
      }
    , betSizings = BetSizings
      { raisePF         = 250
      , cbetFlop        = 80
      , betPostF        = 80
      , betDistribution = 40
      , raisePostF      = 285
      }
  }
