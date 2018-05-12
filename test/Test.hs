import Poker.Logic.Calculations
import Poker.Logic.Types

main :: IO ()
main = do
  putStrLn ("\n" ++ show (computeHandRank hand00))
  putStrLn ("\n" ++ show (computeHandRank hand01))
  putStrLn ("\n" ++ show (computeHandRank hand02))
  putStrLn ("\n" ++ show (computeHandRank hand03))
  putStrLn ("\n" ++ show (computeHandRank hand04))
  putStrLn ("\n" ++ show (computeHandRank hand05))

-- | Test cases for combinations.
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

-- 4x T
hand05 :: [Card]
hand05 =
    [ Card Ten Hearts
    , Card Deuce Spades
    , Card Ten   Spades
    , Card Ten   Clubs
    , Card King  Spades
    , Card Jack  Spades
    , Card Ten   Hearts]
