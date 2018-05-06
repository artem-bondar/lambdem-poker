-- | Module for operating with card dealing.
module Poker.Logic.Dealer where

import System.Random (StdGen, randomR)

import Poker.Logic.Types

-- | Deal cards from deck to players.
dealPlayers :: [Player] -> StdGen -> Deck -> ([Player], (StdGen, Deck))
dealPlayers players randomizer deck = 
    let randomResult = getNHands (length players) (randomizer, deck)
    in (zipWith deal players (fst randomResult), snd randomResult)
    where
        deal player twoCards = player { hand = Just twoCards }

-- | Get one random card from a deck.
getCard :: StdGen -> Deck -> (Card, (StdGen, Deck))
getCard randomizer deck =
    let (deckFirstPart, deckSecondPart) = splitAt index (cards deck)
        randomResult = randomR (1, size deck) randomizer
        index        = fst randomResult
        newGen       = snd randomResult    
    in ((cards deck) !! index, (newGen, Deck
        { size  = size deck - 1
        , cards = deckFirstPart ++ (tail deckSecondPart)
        }))

-- | Get hand from a deck.
getHand :: StdGen -> Deck -> ((Card, Card), (StdGen, Deck))
getHand randomizer deck =
    let first     = getCard randomizer deck
        second    = getCard (fst $ snd first) (snd $ snd first)
    in ((fst first, fst second), snd second)

-- | Get n hands from a deck.
getNHands :: Int -> (StdGen, Deck) -> ([(Card, Card)], (StdGen, Deck))
getNHands 0 x = ([], x)
getNHands n (randomizer, deck) =
    let randomResult = getHand randomizer deck
        nextResult   = getNHands (n - 1) (snd randomResult)
    in (fst randomResult : fst nextResult, snd  nextResult)
