-- | Module to deal with computation related to poker combinations.
module Poker.Logic.Calculations where

import Data.List (sort)

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Functions to operate with cards
-------------------------------------------------------------------------------

-- | Return if card array is a straight and kicker card.
checkStraight :: [Card] -> (Bool, CardRank)
checkStraight cards = (hasStraight, kickerCard)
  where
    numCards       = cardToStraightNumList cards
    cardWithLength = zip numCards (getSequencesLength numCards)
    hasStraight    = or (map (\x -> snd x >= 5) cardWithLength)
    kickerCard     = toEnum (4 + fst (foldl1 (\x y -> if (snd x == 5)
      then x else y) cardWithLength))

-- | Return hand rank and kicker cards depending on card array.
computeHandRank :: [Card] -> (HandRank, [Card])
computeHandRank cards = (High_card, [])

-- | Compute combination from hand and board.
computeCombination :: Maybe (Card, Card) -> [Card] -> Combination
computeCombination handCards board = Combination
  { handRank  = fst handRankComputations
  , structure = allCards
  , kicker    = snd handRankComputations }
  where
    handRankComputations = computeHandRank allCards
    allCards = case handCards of
      Nothing    -> board
      Just cards -> [fst cards, snd cards] ++ board

-------------------------------------------------------------------------------
-- * Functions to operate with combinations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Convert card list to sort int list with removed duplicates, Ace card is
--   counted twice for A2345 straights.
cardToStraightNumList :: [Card] -> [Int]
cardToStraightNumList cards =
  removeDuplicates . sort $ addAce (map (\card -> fromEnum $ cardRank card) cards)
  where
    addAce list   = if (fromEnum Ace `elem` list)
      then (-1 : list)
      else list
    removeDuplicates list = foldl (\seen x ->
      if x `elem` seen
        then seen
        else seen ++ [x])
      [] list

-- | Get length of sequence that starts from head element of list.
getSequenceLength :: [Int] -> Int
getSequenceLength []     = 0
getSequenceLength [_]    = 0
getSequenceLength (x:xs) = if (succ x /= head xs)
  then 0
  else 1 + getSequenceLength xs

-- | Convert int list to list that contain max sequence starting from each element.
getSequencesLength :: [Int] -> [Int]
getSequencesLength [] = []
getSequencesLength (x:xs) = (1 + getSequenceLength (x:xs) : getSequencesLength xs)
