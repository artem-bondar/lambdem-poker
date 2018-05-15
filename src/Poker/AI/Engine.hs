-- | Here is located all functions related to AI computations.
module Poker.AI.Engine where

import Data.List (sort)
import System.Random (StdGen, mkStdGen, randomR)

import Poker.AI.PlayStyles (getTelephonePlaystyle)
import Poker.AI.Types

import Poker.Logic.Calculations (computeCombination)
import Poker.Logic.Types.Cards
import Poker.Logic.Types.Game

-------------------------------------------------------------------------------
-- * Core functions for simulating AI thinking
-------------------------------------------------------------------------------

-- | Main function that return AI move and updated AI data depending on it's
--   playstyle, incoming bet, blind size, pot and street.
calculateAIMove :: Player -> Int -> Int -> Int -> Street -> (Move, AIPlayer)
calculateAIMove player bet bb pot street
  | street == Preflop = case action preFlopMove == Raised of
      True  -> (preFlopMove, aiDataRaw { madePFR = True  })
      False -> (preFlopMove, aiDataRaw { madePFR = False })
  | street == Flop = case action postFlopMove == Raised of
      True  -> (postFlopMove, aiDataRaw { madeCbet = True  })
      False -> (postFlopMove, aiDataRaw { madeCbet = False })
  | street == Flop = case action postFlopMove == Raised of
      True  -> (postFlopMove, aiDataRaw { madeBarrel = True  })
      False -> (postFlopMove, aiDataRaw { madeBarrel = False })
  | otherwise = (postFlopMove, aiDataRaw)
  where
    preFlopMove = calculatePreFlopMove (cards aiDataRaw) (playStyle aiDataRaw)
      (betSize $ move player) bet (balance player) bb
    aiDataRaw = case aiData player of
      Nothing       -> AIPlayer [] getTelephonePlaystyle False False False (mkStdGen 0)
      Just handData -> handData
    combination = computeCombination (hand player) (tail . tail $ cards aiDataRaw)
    raisedLast  = case street of
      Flop  -> madePFR    aiDataRaw
      Turn  -> madeCbet   aiDataRaw
      River -> madeBarrel aiDataRaw
      _     -> False
    postFlopResults = calculatePostFlopMove combination (playStyle aiDataRaw)
      (betSize $ move player) bet (balance player) pot street raisedLast (rng aiDataRaw)
    postFlopMove = fst postFlopResults

-- | Calculate move on preflop depending on hand, playstyle, made bet, incoming bet,
--   max bet and blind size.
calculatePreFlopMove :: [Card] -> PlayStyle -> Int -> Int -> Int -> Int -> Move
calculatePreFlopMove handPF playstyle madeBet bet maxBet bb
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
    suggestedMove = suggestPFMove handPF (pfHandPower playstyle)
    raiseBet      = (bet * (raisePF $ betSizings playstyle)) `div` 100
    foldMove      = Move Folded  madeBet
    checkMove     = Move Checked bet
    callMove      = if (bet <= maxBet)
      then Move Called    bet
      else Move All_In_ed bet
    raiseMove     = if (raiseBet <= maxBet)
      then Move Raised    raiseBet
      else Move All_In_ed raiseBet

-- | Calculate move on postflop depending on combination, playstyle, made bet, incoming bet,
--   max bet, pot size, street and last move.
calculatePostFlopMove :: Combination -> PlayStyle -> Int -> Int -> Int -> Int
                                     -> Street -> Bool -> StdGen -> (Move, StdGen)
calculatePostFlopMove combination playstyle madeBet bet maxBet pot
                                  street cbetted randomizer
  | bet == 0 = case handStrength of
      Trash_Hand -> (checkMove, randomizer)
      _          -> if (cbetted) 
        then tryCbet
        else tryBetCheck
  | betSizeType == Small_Bet = case handStrength of
      Trash_Hand  -> (foldMove, randomizer)
      Weak_Hand   -> tryCallFold
      Medium_Hand -> tryCallFold
      _           -> tryRaiseCall
  | betSizeType == Medium_Bet = case handStrength of
      Trash_Hand  -> (foldMove, randomizer)
      Weak_Hand   -> (foldMove, randomizer)
      Medium_Hand -> tryCallFold
      Strong_Hand -> tryCallFold
      _           -> tryRaiseCall
  | betSizeType == Big_Bet = case handStrength of
      Monster_Hand -> tryRaiseCall
      Strong_Hand  -> tryCallFold
      _            -> (foldMove, randomizer)
  | otherwise = case handStrength of
      Monster_Hand -> tryRaiseCall
      _            -> (foldMove, randomizer)
  where
    betSizeType   = evalBet ((bet * 100) `div` pot) (betRangePostF playstyle)
    handStrength  = evalHand combination (handPower playstyle)
    getPercent range power = case power of
      Trash_Hand   -> 0
      Weak_Hand    -> smallBet range
      Medium_Hand  -> mediumBet range
      Strong_Hand  -> bigBet range
      Monster_Hand -> hugeBet range
    getMoveRange mv = case mv of
      Bet   -> betRangePostF playstyle
      Call  -> callRangePostF playstyle
      _     -> raiseRangePostF playstyle
    moveSucces mv = checkChance (getPercent (getMoveRange mv) handStrength) randomizer
    betBet        = (pot * (fst $ randomR
      (betPostF (betSizings playstyle) - betDistribution (betSizings playstyle),
       betPostF (betSizings playstyle) + betDistribution (betSizings playstyle)) randomizer)) `div` 100
    raiseBet      = (bet * (raisePostF $ betSizings playstyle)) `div` 100
    foldMove      = Move Folded  madeBet
    checkMove     = Move Checked 0
    callMove      = if (bet <= maxBet)
      then Move Called    bet
      else Move All_In_ed bet
    betMove       = if (betBet <= maxBet)
      then Move Raised    betBet
      else Move All_In_ed betBet
    raiseMove     = if (raiseBet <= maxBet)
      then Move Raised    raiseBet
      else Move All_In_ed raiseBet
    cbetChance = case street of
        Flop  -> cbet      playstyle
        Turn  -> sndBarrel playstyle
        _     -> trdBarrel playstyle
    cbetSucces = checkChance cbetChance randomizer
    tryCbet = if (fst cbetSucces)
      then (betMove,   snd $ cbetSucces)
      else (checkMove, snd $ cbetSucces)
    tryBetCheck = if (fst $ moveSucces Bet)
      then (betMove,   snd $ moveSucces Bet)
      else (checkMove, snd $ moveSucces Bet)
    tryCallFold = if (fst $ moveSucces Call)
      then (callMove, snd $ moveSucces Call)
      else (foldMove, snd $ moveSucces Call)
    tryRaiseCall = if (fst $ moveSucces Raise)
      then (raiseMove, snd $ moveSucces Raise)
      else (callMove,  snd $ moveSucces Raise)

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Update AIPlayer data for player.
updateAIData :: Player -> [Card] -> Player
updateAIData player board = player { aiData = newAiData }
  where
    playerHand = case hand player of
      Nothing    -> []
      Just crds  -> [fst crds, snd crds]
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
checkRange handPF range
  | isPaired  = fst ranks `elem` (pairedRange    range)
  | isSuited  =     ranks `elem` (suitedRange    range)
  | otherwise =     ranks `elem` (offsuitedRange range)
  where
    fstCard  = head $        sort handPF
    sndCard  = head . tail $ sort handPF
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
  | combination < weakHand    range = Trash_Hand
  | combination < mediumHand  range = Weak_Hand
  | combination < strongHand  range = Medium_Hand
  | combination < monsterHand range = Strong_Hand
  | otherwise                       = Monster_Hand

-- | Return suggested move for hand on preflop.
suggestPFMove :: [Card] -> HandRangePF -> ActionType
suggestPFMove handPF range
  | checkRange handPF (pushRange  range) = All_In
  | checkRange handPF (raiseRange range) = Raise
  | checkRange handPF (pfrRange   range) = Bet
  | checkRange handPF (vpipRange  range) = Call
  | otherwise                            = Fold
