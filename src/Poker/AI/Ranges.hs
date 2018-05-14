-- | Contains ranges of poker hands.
module Poker.AI.Ranges where

import Poker.AI.Types

import Poker.Logic.Types.Cards

-------------------------------------------------------------------------------
-- * Balanced ranges
-------------------------------------------------------------------------------

-- | Best 2.6% hands for all-in in any situations.
bestPremium :: CardRange
bestPremium = CardRange
 { suitedRange = [(Ace, King)]
  , pairedRange = [Ace, King, Queen]
  , offsuitedRange = [(Ace, King)]
 }

-- | Best 4.2% hands recommended for preflop all-in.
highPremium :: CardRange
highPremium = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen)]
  , pairedRange = [Ace, King, Queen, Jack]
  , offsuitedRange = [(Ace, King), (Ace, Queen)]
 }

-- | Premium 7.5% hands that dominates on preflop
premium :: CardRange
premium = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack)]
 }

-- | 10.9% range
lowPremium :: CardRange
lowPremium = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (King, Queen), (King, Jack)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Ace, Ten)]
 }

-- | 16.6% range, all broadway, 55+
broadwayToFives :: CardRange
broadwayToFives = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (King, Queen), (King, Jack), (King, Ten), (Queen, Jack), (Queen, Ten), (Jack, Ten)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten)]
 }

-- | EP 6max PFR, 12.7%
epPFR :: CardRange
epPFR = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (King, Queen), (King, Jack), (King, Ten), (Queen, Jack), (Queen, Ten), (Jack, Ten)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Ace, Ten)]
 }

-- | MP 6 max PFR, 20.4%
mpPFR :: CardRange
mpPFR = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Jack, Ten), (Jack, Nine), (Ten, Nine)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine)]
 }

-- | CO 6max PFR, 31.1%
coPFR :: CardRange
coPFR = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (King, Seven), (King, Six), (King, Five), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Queen, Eight), (Queen, Seven), (Jack, Ten), (Jack, Nine), (Jack, Eight), (Ten, Nine), (Ten, Eight), (Nine, Eight)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine), (King, Nine), (Queen, Nine), (Jack, Nine), (Ten, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five)]
 }

-- | BTN 6max PFR, 44.2%
btnPFR :: CardRange
btnPFR = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (King, Seven), (King, Six), (King, Five), (King, Four), (King, Three), (King, Deuce), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Queen, Eight), (Queen, Seven), (Queen, Six), (Queen, Five), (Queen, Four), (Jack, Ten), (Jack, Nine), (Jack, Eight), (Jack, Seven), (Jack, Six), (Ten, Nine), (Ten, Eight), (Ten, Seven), (Ten, Six), (Nine, Eight), (Nine, Seven), (Eight, Seven)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Deuce]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine), (King, Nine), (Queen, Nine), (Jack, Nine), (Ten, Nine), (Ace, Eight), (King, Eight), (Queen, Eight), (Jack, Eight), (Ten, Eight), (Nine, Eight), (Ace, Seven), (King, Seven), (Ace, Six), (King, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce)]
 }

-- | Half of all hands, 50.7%
halfOpen :: CardRange
halfOpen = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (King, Seven), (King, Six), (King, Five), (King, Four), (King, Three), (King, Deuce), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Queen, Eight), (Queen, Seven), (Queen, Six), (Queen, Five), (Queen, Four), (Queen, Three), (Queen, Deuce), (Jack, Ten), (Jack, Nine), (Jack, Eight), (Jack, Seven), (Jack, Six), (Jack, Five), (Jack, Four), (Ten, Nine), (Ten, Eight), (Ten, Seven), (Ten, Six), (Nine, Eight), (Nine, Seven), (Eight, Seven)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Deuce]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine), (King, Nine), (Queen, Nine), (Jack, Nine), (Ten, Nine), (Ace, Eight), (King, Eight), (Queen, Eight), (Jack, Eight), (Ten, Eight), (Nine, Eight), (Ace, Seven), (King, Seven), (Queen, Seven), (Jack, Seven), (Ten, Seven), (Ace, Six), (King, Six), (Ace, Five), (King, Five), (Ace, Four), (Ace, Three), (Ace, Deuce)]
 }

-- | 60.8%
sixtyOpen :: CardRange
sixtyOpen = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (King, Seven), (King, Six), (King, Five), (King, Four), (King, Three), (King, Deuce), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Queen, Eight), (Queen, Seven), (Queen, Six), (Queen, Five), (Queen, Four), (Queen, Three), (Queen, Deuce), (Jack, Ten), (Jack, Nine), (Jack, Eight), (Jack, Seven), (Jack, Six), (Jack, Five), (Jack, Four), (Jack, Three), (Jack, Deuce), (Ten, Nine), (Ten, Eight), (Ten, Seven), (Ten, Six), (Ten, Five), (Ten, Four), (Ten, Three), (Nine, Eight), (Nine, Seven), (Nine, Six), (Nine, Five), (Eight, Seven), (Eight, Six), (Eight, Five), (Seven, Six), (Seven, Five), (Seven, Four), (Six, Five), (Six, Four), (Five, Four)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Deuce]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine), (King, Nine), (Queen, Nine), (Jack, Nine), (Ten, Nine), (Ace, Eight), (King, Eight), (Queen, Eight), (Jack, Eight), (Ten, Eight), (Nine, Eight), (Ace, Seven), (King, Seven), (Queen, Seven), (Jack, Seven), (Ten, Seven), (Nine, Seven), (Eight, Seven), (Ace, Six), (King, Six), (Queen, Six), (Ace, Five), (King, Five), (Queen, Five), (Ace, Four), (King, Four), (Ace, Three), (King, Three), (Ace, Deuce), (King, Deuce)]
 }

-- | 70.1%
seventyOpen :: CardRange
seventyOpen = CardRange
 { suitedRange = [(Ace, King), (Ace, Queen), (Ace, Jack), (Ace, Ten), (Ace, Nine), (Ace, Eight), (Ace, Seven), (Ace, Six), (Ace, Five), (Ace, Four), (Ace, Three), (Ace, Deuce), (King, Queen), (King, Jack), (King, Ten), (King, Nine), (King, Eight), (King, Seven), (King, Six), (King, Five), (King, Four), (King, Three), (King, Deuce), (Queen, Jack), (Queen, Ten), (Queen, Nine), (Queen, Eight), (Queen, Seven), (Queen, Six), (Queen, Five), (Queen, Four), (Queen, Three), (Queen, Deuce), (Jack, Ten), (Jack, Nine), (Jack, Eight), (Jack, Seven), (Jack, Six), (Jack, Five), (Jack, Four), (Jack, Three), (Jack, Deuce), (Ten, Nine), (Ten, Eight), (Ten, Seven), (Ten, Six), (Ten, Five), (Ten, Four), (Ten, Three), (Ten, Deuce), (Nine, Eight), (Nine, Seven), (Nine, Six), (Nine, Five), (Nine, Four), (Nine, Three), (Eight, Seven), (Eight, Six), (Eight, Five), (Eight, Four), (Seven, Six), (Seven, Five), (Seven, Four), (Six, Five), (Six, Four), (Six, Three), (Five, Four), (Five, Three), (Four, Three)]
  , pairedRange = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Deuce]
  , offsuitedRange = [(Ace, King), (Ace, Queen), (King, Queen), (Ace, Jack), (King, Jack), (Queen, Jack), (Ace, Ten), (King, Ten), (Queen, Ten), (Jack, Ten), (Ace, Nine), (King, Nine), (Queen, Nine), (Jack, Nine), (Ten, Nine), (Ace, Eight), (King, Eight), (Queen, Eight), (Jack, Eight), (Ten, Eight), (Nine, Eight), (Ace, Seven), (King, Seven), (Queen, Seven), (Jack, Seven), (Ten, Seven), (Nine, Seven), (Eight, Seven), (Ace, Six), (King, Six), (Queen, Six), (Jack, Six), (Ten, Six), (Nine, Six), (Eight, Six), (Seven, Six), (Ace, Five), (King, Five), (Queen, Five), (Jack, Five), (Ace, Four), (King, Four), (Queen, Four), (Ace, Three), (King, Three), (Queen, Three), (Ace, Deuce), (King, Deuce)]
 }

-------------------------------------------------------------------------------
-- * Special ranges
-------------------------------------------------------------------------------
