-- | Module to deal with computation related to poker combinations.
module Poker.Logic.Calculations where

import Data.List (sort)

import Poker.Logic.Types

import Debug.Trace

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
    kickerCardNum  = fst (foldl1 (\x y -> if (snd x == 5)
      then x else y) cardWithLength)
    kickerCard = if (kickerCardNum == -1)
      then Five
      else toEnum $ kickerCardNum + 4

-- | Return if card array is a flush and 5 kicker cards.
checkFlush :: [Card] -> (Bool, [CardRank])
checkFlush cards = (hasFlush, map toEnum kickerCards)
  where
    numSuits    = countSuits cards
    hasFlush    = or (map (\x -> x >= 5) numSuits)
    kickerCards = case hasFlush of
      True  -> takeBest5 (map (\card -> fromEnum $ cardRank card) $
        filter (\card -> numSuits !! (fromEnum $ suit card) >= 5) cards)
      False -> []
    takeBest5 crds
      | length crds == 7 = tail . tail $ sort crds
      | length crds == 6 = tail $ sort crds
      | otherwise        = crds

-- | Return hand rank and kicker cards depending on card array.
computeHandRank :: [Card] -> (HandRank, ([CardRank], [CardRank]))
computeHandRank cards = (High_card, ([], []))

-- | Compute combination from hand and board.
computeCombination :: Maybe (Card, Card) -> [Card] -> Combination
computeCombination handCards board = Combination
  { handRank  = fst handRankComputations
  , structure = fst $ snd handRankComputations
  , kicker    = snd $ snd handRankComputations }
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

-- | Count amount of each suit in card list.
countSuits :: [Card] -> [Int]
countSuits cards = foldl (\(d:c:h:s:v) card -> case suit card of
  Diamonds -> (d + 1:c:h:s:v)
  Clubs    -> (d:c + 1:h:s:v)
  Hearts   -> (d:c:h + 1:s:v)
  Spades   -> (d:c:h:s + 1:v)) [0, 0, 0, 0] cards
