-- | Module for operating with card dealing.
module Poker.Logic.Dealer where

import System.Random (StdGen, randomR)

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Operations with cards
-------------------------------------------------------------------------------

-- | Deal cards from deck to players.
dealPlayers :: [Player] -> StdGen -> Deck -> ([Player], (StdGen, Deck))
dealPlayers players randomizer deck = 
  let randomResult = performN getHand (length players) (randomizer, deck)
  in (zipWith deal players (fst randomResult), snd randomResult)
  where
    deal player twoCards = player { hand = Just twoCards }

-- | Get one random card from a deck.
getCard :: StdGen -> Deck -> (Card, (StdGen, Deck))
getCard randomizer deck =
  let (deckFirstPart, deckSecondPart) = splitAt index (body deck)
      randomResult = randomR (0, size deck - 1) randomizer
      index        = fst randomResult
      newGen       = snd randomResult    
  in ((body deck) !! index, (newGen, Deck
    { size  = size deck - 1
    , body = deckFirstPart ++ (tail deckSecondPart)
    }))

-- | Get hand from a deck.
getHand :: StdGen -> Deck -> ((Card, Card), (StdGen, Deck))
getHand randomizer deck =
  let first  = getCard randomizer deck
      second = getCard (fst $ snd first) (snd $ snd first)
  in ((fst first, fst second), snd second)

-- | Get n entities from a deck.
performN :: (StdGen -> Deck -> (a, (StdGen, Deck))) -> Int ->
  (StdGen, Deck) -> ([a], (StdGen, Deck))
performN _ 0 x = ([], x)
performN performOnce n (randomizer, deck) =
  let randomResult = performOnce randomizer deck
      nextResult   = performN performOnce (n - 1) (snd randomResult)
  in (fst randomResult : fst nextResult, snd nextResult)

-- | Deal board cards depending on street.
dealBoard :: StdGen -> Deck -> [Card] -> Street -> ([Card], (StdGen, Deck))
dealBoard randomizer deck [] Flop = performN getCard 3 (randomizer, deck)
dealBoard randomizer deck board street
  | street == Turn  && length board < 4 ||
    street == River && length board < 5 = (board ++ [fst randomResult], snd randomResult)
  | otherwise = (board, (randomizer, deck))
  where
    randomResult = getCard randomizer deck

-------------------------------------------------------------------------------
-- * Operations with players
-------------------------------------------------------------------------------

-- | Take blind from player and mark bankrupted players.
takeBlind :: Player -> Int -> Player
takeBlind player blind
    | balance player == 0     = player
      { move = Move Bankrupted 0 }
    | balance player <= blind = player
      { move = Move All_In_ed (balance player) }
    | otherwise               = player
      { move = Move Raised blind }

-- | Take blinds from players and mark bankrupted players.
takeBlinds :: [Player] -> Int -> [Player]
takeBlinds [] _      = []
takeBlinds (p:ps) bb = 
  let takeBB player blind = case position player of
        SB -> takeBlind player (blind `div` 2)
        BB -> takeBlind player blind
        _  -> if (balance player == 0)
          then player { move = Move Bankrupted 0 }
          else player  
  in (takeBB p bb : takeBlinds ps bb)

-- | Hide hand depending on player type and settings.
hideHands :: [Player] -> [Player]
hideHands players = map
    (\player -> case control player of
        Human -> player { hideHand = hideHumaHand }
        AI    -> player { hideHand = hideAIhand   })
    players

-- | Open all hands.
openHands :: [Player] -> [Player]
openHands players = map (\player -> player { hideHand = False }) players

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Time consumed to deal cards.
dealTime :: Float
dealTime = 0.60

-- | Time consumed to post blinds.
postTime :: Float
postTime = 1.0

-- | Hide AI hand during bet rounds.
hideAIhand :: Bool
hideAIhand = True

-- | Hide human hand during bet rounds.
hideHumaHand :: Bool
hideHumaHand = False
