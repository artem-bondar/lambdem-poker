-- | Here is located all functions related to AI computations.
module Poker.AI.Engine where

import Data.List (sort)
import System.Random (StdGen, mkStdGen, randomR)

import Poker.AI.PlayStyles (getTelephonePlaystyle)
import Poker.AI.Types

import Poker.Interface.Types

import Poker.Logic.Types.Cards
import Poker.Logic.Types.Game

-------------------------------------------------------------------------------
-- * Core functions for simulating AI thinking
-------------------------------------------------------------------------------

-- | Main function that return AI move and updated AI data depending on it's
--   playstyle, incoming bet, blind size, pot and street.
calculateAIMove :: Player -> Int -> Int -> Int -> Street -> (Move, AIPlayer)
calculateAIMove player bet bb pot street
  | street == Preflop = case action flopMove == Raised && madePfr aiDataRaw == False of
      True  -> (flopMove, aiDataRaw { madePfr = True  })
      False -> (flopMove, aiDataRaw { madePfr = False })
  | otherwise = (Move Folded 0, aiDataRaw)
  where
    flopMove  = calculateFlopMove (cards aiDataRaw) (playStyle aiDataRaw)
      (betSize $ move player) bet (balance player) bb
    aiDataRaw = case aiData player of
      Nothing       -> AIPlayer [] getTelephonePlaystyle False False False False (mkStdGen 0)
      Just handData -> handData

-- | Calculate move on flop depending on hand, playstyle, made bet, incoming bet, max bet
--   blind size and bet sizings.
calculateFlopMove :: [Card] -> PlayStyle -> Int -> Int -> Int -> Int -> Move
calculateFlopMove hand playstyle madeBet bet maxBet bb
  | bet == bb = case suggestedMove of
    Fold -> if (madeBet == bb)
      then checkMove
      else foldMove
    Call -> if (madeBet == bb)
      then checkMove
      else callMove
    _    -> raiseMove
  | betSizeType == Small_Bet = case suggestedMove of
    Fold -> foldMove
    Call -> foldMove
    Bet  -> callMove
    _    -> raiseMove
  | betSizeType == Medium_Bet = case suggestedMove of
    Fold  -> foldMove
    Call  -> foldMove
    Bet   -> callMove
    Raise -> callMove
    _     -> raiseMove
  | betSizeType == Big_Bet = case suggestedMove of
    Fold  -> foldMove
    Call  -> foldMove
    Bet   -> foldMove
    Raise -> callMove
    _     -> raiseMove
  | otherwise = case suggestedMove of
    All_In -> raiseMove
    _      -> foldMove
  where
    betSizeType   = evalBet (bet `div` bb) (betRangePF playstyle)
    suggestedMove = suggestPFMove hand (pfHandPower playstyle)
    raiseBet      = (bet * (raisePF $ betSizings playstyle)) `div` 100
    foldMove      = Move Folded  madeBet
    checkMove     = Move Checked bet
    callMove      = Move Called  bet
    raiseMove     = if (raiseBet <= maxBet)
      then Move Raised    raiseBet
      else Move All_In_ed raiseBet

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Update AIPlayer data for player.
updateAIData :: Player -> [Card] -> Player
updateAIData player board = player { aiData = newAiData }
  where
    playerHand = case hand player of
      Nothing    -> []
      Just cards -> [fst cards, snd cards]
    newAiData  = case aiData player of
      Nothing       -> Nothing
      Just handData -> Just (handData {cards = playerHand ++ board })

-- | Write changes to AI data to player on given position.
writeAIDataChange :: [Player] -> Position -> AIPlayer -> [Player]
writeAIDataChange players pos newAiData = map
  (\player -> case position player == pos of
      True  -> player { aiData = Just newAiData }
      False -> player)
  players  

-- | Return if chance in range 0-100 succeed.
checkChance :: Int -> StdGen -> (Bool, StdGen)
checkChance chance randomizer = (fst randomResult <= chance, snd randomResult)
  where
    randomResult = randomR (0, 100) randomizer

-- | Return if hand is in range.
checkRange :: [Card] -> CardRange -> Bool
checkRange cards range
  | isPaired  = fst ranks `elem` (pairedRange    range)
  | isSuited  =     ranks `elem` (suitedRange    range)
  | otherwise =     ranks `elem` (offsuitedRange range)
  where
    fstCard  = head $        sort cards
    sndCard  = head . tail $ sort cards
    ranks    = (cardRank fstCard, cardRank sndCard)
    isPaired = fst ranks == snd ranks
    isSuited = suit fstCard == suit sndCard

-- | Return type of bet.
evalBet :: Int -> BetRange -> BetType
evalBet bet range
  | bet <= smallBet  range = Small_Bet
  | bet <= mediumBet range = Medium_Bet
  | bet <= bigBet    range = Big_Bet
  | otherwise              = Huge_Bet

-- | Return strength of combination.
evalHand :: Combination -> CombinationRange -> HandPower
evalHand combination range
  | combination <= weakHand   range = Weak_Hand
  | combination <= mediumHand range = Medium_Hand
  | combination <= strongHand range = Strong_Hand
  | otherwise                       = Monster_Hand

-- | Return suggested move for hand on preflop.
suggestPFMove :: [Card] -> HandRangePF -> ActionType
suggestPFMove hand range
  | checkRange hand (pushRange  range) = All_In
  | checkRange hand (raiseRange range) = Raise
  | checkRange hand (pfrRange   range) = Bet
  | checkRange hand (vpipRange  range) = Call
  | otherwise                          = Fold
