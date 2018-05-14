-- | Here is located all functions related to AI computations.
module Poker.AI.Engine where

import Data.List (sort)
import System.Random (StdGen, randomR)

import Poker.AI.Types

import Poker.Interface.Types

import Poker.Logic.Types.Cards
import Poker.Logic.Types.Game

-------------------------------------------------------------------------------
-- * Core functions for simulating AI thinking
-------------------------------------------------------------------------------

-- | Main function that return AI move depending on it's playstyle, 
--   incoming bet and street.
calculateAIMove :: Player -> Int -> Street -> Move
calculateAIMove player bet street
  | street == Preflop = Move Folded 0
  | otherwise = Move Folded 0

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
