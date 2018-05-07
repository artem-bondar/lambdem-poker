-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Position processing functions
-------------------------------------------------------------------------------

-- | Set first player to perform action depending on street.
setFirstPlayer :: [Player] -> Street -> [Player]
setFirstPlayer players street = 
    toggleNewActivePlayer players (getFirstPosition (length players) street)

-- | Set active player depending to given position
--   and unsets previous active player.
toggleNewActivePlayer :: [Player] -> Position -> [Player]
toggleNewActivePlayer players pos = map
    (\player -> case position player == pos of
        True  -> player { active = True  }
        False -> player { active = False })
    players    

-- | Return type of active player.
getActivePlayerType :: [Player] -> PlayerType
getActivePlayerType players = case active $ head players of
    True  -> control $ head players
    False -> getActivePlayerType $ tail players

-- | Return first position depending on amount of players and street.
getFirstPosition :: Int -> Street -> Position
getFirstPosition amountOfPlayers street = case amountOfPlayers of
    2 -> if (street == Preflop)
            then BB
            else SB
    _ -> UTG

-- | Return next position depending on amount of players and street.
getNextPositon :: Int -> Street -> Position -> Position
getNextPositon amountOfPlayers street previousPosition =
    case amountOfPlayers of
        2 -> if (street == Preflop)
                then SB
                else BB
        _ -> MP

-- | Return last position depending on amount of players and street.
getLastPosition :: Int -> Street -> Position
getLastPosition amountOfPlayers street = case amountOfPlayers of
    2 -> if (street == Preflop)
            then SB
            else BB
    _ -> BTN

-------------------------------------------------------------------------------
-- * Players actions processing functions
-------------------------------------------------------------------------------
