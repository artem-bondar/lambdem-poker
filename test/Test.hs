import Poker.Logic.Calculations
import Poker.Logic.Types

main :: IO ()
main = do
  putStrLn . show $ checkStraight hand00 

-- | Test cases for combinations.
hand00 =
  [ Card Deuce Spades
  , Card Three Spades
  , Card Four  Spades
  , Card Five  Spades
  , Card Six   Spades
  , Card Seven Spades]
