import Poker.Logic.Calculations
import Poker.Logic.Types.Cards

main :: IO ()
main = do
--   putStrLn ("\n  1: " ++ show (computeHandRank hand00r))
--   putStrLn ("\n  2: " ++ show (computeHandRank hand00))
--   putStrLn ("\n  3: " ++ show (computeHandRank hand01))
--   putStrLn ("\n  4: " ++ show (computeHandRank hand02))
--   putStrLn ("\n  5: " ++ show (computeHandRank hand03))
--   putStrLn ("\n  6: " ++ show (computeHandRank hand04))
--   putStrLn ("\n  7: " ++ show (computeHandRank hand05))
--   putStrLn ("\n  8: " ++ show (computeHandRank hand06))
--   putStrLn ("\n  9: " ++ show (computeHandRank hand07))
--   putStrLn ("\n 10: " ++ show (computeHandRank hand08))
--   putStrLn ("\n 11: " ++ show (computeHandRank hand09))
--   putStrLn ("\n 12: " ++ show (computeHandRank hand10))
--   putStrLn ("\n 13: " ++ show (computeHandRank hand11))
--   putStrLn ("\n 14: " ++ show (computeHandRank hand12))
--   putStrLn ("\n 14: " ++ show (computeHandRank hand13))
--   putStrLn ("\n" ++ show (getCombination hand00r > getCombination hand00))
--   putStrLn ("\n" ++ show (getCombination hand00  > getCombination hand01))
--   putStrLn ("\n" ++ show (getCombination hand02  > getCombination hand03))
--   putStrLn ("\n" ++ show (getCombination hand03  > getCombination hand04))
--   putStrLn ("\n" ++ show (getCombination hand05  > getCombination hand06))
--   putStrLn ("\n" ++ show (getCombination hand07  > getCombination hand07c))
--   putStrLn ("\n" ++ show (getCombination hand10  > getCombination hand12))
--   putStrLn ("\n" ++ show (getCombination hand12  < getCombination hand12c))
--   putStrLn ("\n" ++ show (getCombination hand09  > getCombination hand09c))
--   putStrLn ("\n" ++ show (getCombination hand08  > getCombination hand08c))
  putStrLn ("\n" ++ show (markWinningCombinations markedCombinations))
  where
    handRankComputations cards = computeHandRank cards
    getCombination cards = Combination
      { handRank  = fst $ handRankComputations cards
      , structure = fst . snd $ handRankComputations cards
      , kicker    = snd . snd $ handRankComputations cards }
    combinations = map getCombination [hand00, hand01, hand02, hand05, hand00r, hand06]
    markedCombinations = zip (replicate (length combinations) True) combinations

-- | Test cases for combinations.

-- T-A Royal Flush
hand00r :: [Card]
hand00r =
  [ Card Deuce Spades
  , Card King Spades
  , Card Four  Spades
  , Card Queen  Spades
  , Card Ten   Spades
  , Card Ace Spades
  , Card Jack Spades]

-- 2-8 Straigh Flush
hand00 :: [Card]
hand00 =
  [ Card Deuce Spades
  , Card Three Spades
  , Card Four  Spades
  , Card Five  Spades
  , Card Six   Spades
  , Card Seven Spades
  , Card Eight Spades]

-- 2-7 Straigh Flush
hand01 :: [Card]
hand01 =
  [ Card Deuce Spades
  , Card Five  Spades
  , Card Six   Spades
  , Card Three Spades
  , Card Four  Spades
  , Card Seven Spades
  , Card Nine  Spades]

-- A-5 Straight
hand02 :: [Card]
hand02 =
  [ Card Deuce Hearts
  , Card Ace   Spades
  , Card King  Spades
  , Card Three Clubs
  , Card Four  Spades
  , Card Seven Spades
  , Card Five  Diamonds]

-- T-A Straight
hand03 :: [Card]  
hand03 =
  [ Card Queen Hearts
  , Card Ace   Spades
  , Card Six   Spades
  , Card King  Clubs
  , Card Ten   Spades
  , Card Jack  Spades
  , Card Eight Hearts]
 
-- 9-K Straight
hand04 :: [Card]
hand04 =
    [ Card Queen Hearts
    , Card Deuce Spades
    , Card Six   Spades
    , Card King  Clubs
    , Card Ten   Spades
    , Card Jack  Spades
    , Card Nine  Hearts]

-- 4x T + K
hand05 :: [Card]
hand05 =
    [ Card Ten Hearts
    , Card Deuce Spades
    , Card Ten   Spades
    , Card Ten   Clubs
    , Card King  Spades
    , Card Jack  Spades
    , Card Ten   Hearts]

-- 4x 5 + 7
hand06 :: [Card]
hand06 =
    [ Card Five  Hearts
    , Card Deuce Spades
    , Card Five  Spades
    , Card Five  Clubs
    , Card Jack  Spades
    , Card Seven Spades
    , Card Five  Hearts]

-- 3/2 Q+6
hand07 :: [Card]
hand07 =
    [ Card Queen Hearts
    , Card Deuce Spades
    , Card Six   Spades
    , Card King  Clubs
    , Card Six   Hearts
    , Card Queen  Spades
    , Card Queen  Diamonds]

-- 3/2 Q+5
hand07c :: [Card]
hand07c =
    [ Card Queen Hearts
    , Card Deuce Spades
    , Card Five   Spades
    , Card King  Clubs
    , Card Five   Hearts
    , Card Queen  Spades
    , Card Queen  Diamonds]

-- 3/2 3+A
hand08 :: [Card]
hand08 =
    [ Card Three Hearts
    , Card Deuce Spades
    , Card Three Spades
    , Card Three Clubs
    , Card King  Spades
    , Card Ace   Spades
    , Card Ace   Hearts]

-- 3/2 3+K
hand08c :: [Card]
hand08c =
    [ Card Three Hearts
    , Card Deuce Spades
    , Card Three Spades
    , Card Three Clubs
    , Card Ace   Spades
    , Card King  Spades
    , Card King  Hearts]

-- 3x 4
hand09 :: [Card]
hand09 =
    [ Card Ten Hearts
    , Card Deuce Spades
    , Card Four  Spades
    , Card Four  Clubs
    , Card King  Spades
    , Card Jack  Spades
    , Card Four  Hearts]

-- 3x 4
hand09c :: [Card]
hand09c =
    [ Card Ten Hearts
    , Card Deuce Spades
    , Card Four  Spades
    , Card Four  Clubs
    , Card Ace   Spades
    , Card Jack  Spades
    , Card Four  Hearts]

-- 2x2 KK+AA
hand10 :: [Card]
hand10 =
    [ Card Queen Hearts
    , Card Deuce Spades
    , Card Ace   Spades
    , Card Ace   Clubs
    , Card Six   Hearts
    , Card King  Spades
    , Card King  Diamonds]

-- 2 8
hand11 :: [Card]
hand11 =
    [ Card Three Hearts
    , Card Deuce Spades
    , Card Eight Spades
    , Card Eight Clubs
    , Card King  Spades
    , Card Seven Spades
    , Card Ace   Hearts]

-- h
hand12 :: [Card]
hand12 =
    [ Card Ten Hearts
    , Card Deuce Spades
    , Card Four  Spades
    , Card Queen Clubs
    , Card King  Spades
    , Card Jack  Spades
    , Card Three Hearts]

-- h
hand12c :: [Card]
hand12c =
    [ Card Ten   Hearts
    , Card Deuce Spades
    , Card Four  Spades
    , Card Queen Clubs
    , Card King  Spades
    , Card Jack  Spades
    , Card Five  Hearts]

-- 2x3
hand13 :: [Card]
hand13 =
    [ Card Ten   Hearts
    , Card Ten   Spades
    , Card Ten   Clubs
    , Card Queen Clubs
    , Card Queen Spades
    , Card Queen Spades
    , Card Five  Hearts]
