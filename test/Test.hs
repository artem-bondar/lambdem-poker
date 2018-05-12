import Poker.Logic.Calculations
import Poker.Logic.Types

main :: IO ()
main = do
  putStrLn ("\n" ++ show (computeHandRank hand00r))
  putStrLn ("\n" ++ show (computeHandRank hand00))
  putStrLn ("\n" ++ show (computeHandRank hand01))
  putStrLn ("\n" ++ show (computeHandRank hand02))
  putStrLn ("\n" ++ show (computeHandRank hand03))
  putStrLn ("\n" ++ show (computeHandRank hand04))
  putStrLn ("\n" ++ show (computeHandRank hand05))
  putStrLn ("\n" ++ show (computeHandRank hand06))
  putStrLn ("\n" ++ show (computeHandRank hand07))
  putStrLn ("\n" ++ show (computeHandRank hand08))
  putStrLn ("\n" ++ show (computeHandRank hand09))
  putStrLn ("\n" ++ show (computeHandRank hand10))
  putStrLn ("\n" ++ show (computeHandRank hand11))
  putStrLn ("\n" ++ show (computeHandRank hand12))

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