-- | Module to deal with computation related to poker combinations.
module Poker.Logic.Calculations where

import Data.List (sort)

import Poker.Logic.Types.Cards

--import Debug.Trace

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

-- | Return if card array is a flush and 5-7 kicker cards.
checkFlush :: [Card] -> (Bool, [CardRank])
checkFlush cards = (hasFlush, map toEnum kickerCards)
  where
    numSuits    = countSuits cards
    hasFlush    = or (map (\x -> x >= 5) numSuits)
    kickerCards = case hasFlush of
      True  -> map (\card -> fromEnum $ cardRank card) $ reverse (
        filter (\card -> numSuits !! (fromEnum $ suit card) >= 5) cards)
      False -> []

-- | Return hand rank and kicker cards depending on card array.
computeHandRank :: [Card] -> (HandRank, ([CardRank], [CardRank]))
computeHandRank cards
  | hasStraightFlush =
    if (straighFlushRank == Ace)
      then (Royal_flush,    ([], []))
      else (Straight_flush, ([straighFlushRank], []))
  | 4 `elem` rankList =
    (Four_of_a_kind, ([toEnum (head $ takeEqualBestN 1 4 rankList)], [fourOfAKindKicker]))
  | 3 `elem` rankList && 2 `elem` rankList =
    (Full_house, ([toEnum (head $ takeEqualBestN 1 3 rankList),
                   toEnum (head $ takeEqualBestN 1 2 rankList)], []))
  | length (takeEqualBestN 2 3 rankList) == 2 =
    (Full_house, ([toEnum (head       $ takeEqualBestN 1 3 rankList),
                   toEnum (head .tail $ takeEqualBestN 2 3 rankList)], []))
  | hasFlush    = (Flush,    (flushRank,     []))
  | hasStraight = (Straight, ([straighRank], []))
  | 3 `elem` rankList =
    (Three_of_a_kind, ([toEnum (head $ takeEqualBestN 1 3 rankList)], threeOfAKingKicker))
  | length (takeEqualBestN 3 2 rankList) > 1 =
    (Two_pair, (map toEnum     (takeEqualBestN 2 2 rankList), [twoPairKicker]))
  | 2 `elem` rankList =
    (One_pair, ([toEnum (head $ takeEqualBestN 1 2 rankList)],
             map toEnum        (takeEqualBestN 3 1 rankList)))
  | otherwise =
    (High_card, (map toEnum (takeEqualBestN 5 1 rankList), []))
  where
    hasFlush    = fst $ checkFlush    cards
    hasStraight = fst $ checkStraight cards
    flushRank   = snd $ checkFlush    cards
    straighRank = snd $ checkStraight cards
    rankList    = countRanks cards 
    hasStraightFlush  = hasFlush &&
                       (fst $ checkStraight (map (\rank -> Card rank Spades) flushRank))
    straighFlushRank  = snd $ checkStraight (map (\rank -> Card rank Spades) flushRank)

    fourOfAKindKicker = toEnum . maximum . concat $ map
      (\x -> takeEqualBestN 1 x rankList) [1, 2, 3]
    threeOfAKingKicker = (map toEnum) . (take 2) . reverse . sort . concat $ map
      (\x -> takeEqualBestN 2 x rankList) [1, 3]
    twoPairKicker      = toEnum $ maximum [head $ takeEqualBestN 1 1 rankList, trdPairRank]
    trdPairRank        = if (length (takeEqualBestN 3 2 rankList) == 3)
      then (takeEqualBestN 3 2 rankList) !! 2
      else -1

-- | Compute combination from hand and board.
computeCombination :: Maybe (Card, Card) -> [Card] -> Combination
computeCombination handCards board = Combination
  { handRank  = fst handRankComputations
  , structure = fst $ snd handRankComputations
  , kicker    = snd $ snd handRankComputations
  }
  where
    handRankComputations = computeHandRank allCards
    allCards = case handCards of
      Nothing    -> board
      Just cards -> [fst cards, snd cards] ++ board

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

-- | Count amount of each rank in card list.
countRanks :: [Card] -> [Int]
countRanks cards = foldl (\ranks card -> addRank ranks $ fromEnum (cardRank card))
  (replicate 13 0) cards
  where
    addRank list pos = fst (splitAt pos list) ++
      (head (snd (splitAt pos list)) + 1:tail (snd (splitAt pos list)))

-- | Return indexes of n last elements that are equal to given.
takeEqualBestN :: Int -> Int -> [Int] -> [Int]
takeEqualBestN n num list = snd . unzip . take n $ reverse
  (filter (\x -> fst x == num) (zip list [0..12]))

-- | Convert combination list to bool list with marked top combinations.
--   Receive combination list zipped with bool list that indicates if
--   the combination require to participate in comparing.
markWinningCombinations :: [(Bool, Combination)] -> [Bool]
markWinningCombinations participateAndCombinations = bitWinners
  where
    filteredWithIndexes = map (\((_,c),i) -> (c,i)) $ filter (\pAc -> fst $ fst pAc)
      (zip participateAndCombinations [0..length participateAndCombinations])
    sorted     = reverse $ sort filteredWithIndexes
    winners    = (head sorted:takeWhile (\(c, _) -> c == fst (head sorted)) (tail sorted))
    bitWinners = foldl (\bitmap index ->
      fst (splitAt index bitmap) ++ [True] ++ tail (snd $ splitAt index bitmap))
      (replicate (length participateAndCombinations) False) (snd $ unzip winners)
