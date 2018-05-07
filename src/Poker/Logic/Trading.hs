-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-- | Set first player to perform action depending on street.
setFirstPlayer :: [Player] -> Street -> [Player]
setFirstPlayer players street = setActivePlayer players firstPositon 
    where
        firstPositon = case length players of
            2 -> if (street == Preflop)
                    then BB
                    else SB
            _ -> UTG

-- | Set active player depending to given position.
setActivePlayer :: [Player] -> Position -> [Player]
setActivePlayer players pos
    | (position $ head players) == pos =
        ((head players) { active = True } : tail players)
    | otherwise =
        (head players : setActivePlayer (tail players) pos)
        