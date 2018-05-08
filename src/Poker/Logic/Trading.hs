-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Position processing functions
-------------------------------------------------------------------------------

-- | Return active player.
--   Isn't safe for [] case.
getActivePlayer :: [Player] -> Player
getActivePlayer players
  | active $ head players = head players
  | otherwise = getActivePlayer (tail players)

-- | Set active player depending to given position
--   and unsets previous active player.
toggleNewActivePlayer :: [Player] -> Position -> [Player]
toggleNewActivePlayer players pos = map
  (\player -> case position player == pos of
      True  -> player { active = True  }
      False -> player { active = False })
  players    

-- | Check if active player is skippable.
checkSkipForActivePlayer :: [Player] -> Bool
checkSkipForActivePlayer [] = False
checkSkipForActivePlayer players
  | active $ head players = case action . move $ head players of
      All_In -> True
      Folded -> True
      _      -> False
  | otherwise = checkSkipForActivePlayer $ tail players

-- | Return first position depending on amount of players and street.
getFirstPosition :: Int -> Street -> Position
getFirstPosition amountOfPlayers street = case amountOfPlayers of
  2 -> if (street == Preflop)
        then SB
        else BB
  _ -> UTG

-- | Return next position depending on amount of players and street.
getNextPositon :: Int -> Street -> Position -> Position
getNextPositon amountOfPlayers street _ = --previousPosition =
  case amountOfPlayers of
    2 -> if (street == Preflop)
          then BB
          else SB
    _ -> MP

-- | Return last position depending on amount of players and street.
getLastPosition :: Int -> Street -> Position
getLastPosition amountOfPlayers street = case amountOfPlayers of
  2 -> if (street == Preflop)
        then BB
        else SB
  _ -> BTN

-------------------------------------------------------------------------------
-- * Players actions processing functions
-------------------------------------------------------------------------------

-- | Return amount of players left in hand.
countInHandPlayers :: [Player] -> Int
countInHandPlayers players = foldl1 (+) (map
  (\player -> case action $ move player of
      Folded    -> 0
      _         -> 1)
  players)

-- | Return maximal bet that occured.
countMaxBet :: [Player] -> Int
countMaxBet players = maximum (map
  (\player -> betSize $ move player)
  players)

-- | Return if repeating of trade is needed.
checkReTrade :: [Player] -> Int -> Bool
checkReTrade players bet = or (map
  (\player ->
      mv player /= No_Action && mv player /= Folded &&
      mv player /= All_In && bt player /= bet)
  players)
  where
    mv p = action  $ move p
    bt p = betSize $ move p

-- | Write move to player on given position.
writeMove :: [Player] -> Position -> Move -> [Player]
writeMove players pos mv = map
  (\player -> case position player == pos of
      True  -> player { move = mv }
      False -> player)
  players    

-- | Return default move to proposed bet size when human didn't made any input.
autoHumanMove :: Player -> Int -> Move
autoHumanMove player bet
  | bet == 0          = Move Checked 0
  | bet == premadeBet = Move Checked bet
  | otherwise         = Move Folded premadeBet
  where
    premadeBet = betSize $ move player

-- | Decrease balance by bet, increase invested by bet, vanish move if can.
applyMoveResults :: [Player] -> [Player]
applyMoveResults players = map
  (\player -> player
    { balance  = balance  player - bet player
    , invested = invested player + bet player
    , move     = case action $ move player of
        Folded -> Move Folded 0
        All_In -> Move All_In 0
        _      -> Move No_Action 0
    })
  players
  where
    bet p = betSize $ move p

-- | Calculate pot.
calculatePot :: [Player] -> Int
calculatePot players = foldl1 (+) (map
  (\player -> invested player)
  players)

-- | Time to get response from AI player.
aiThinkTime :: Float
aiThinkTime = 2.0

-- | Time to get response from human player.
humanThinkTime :: Float
humanThinkTime = 2.0
