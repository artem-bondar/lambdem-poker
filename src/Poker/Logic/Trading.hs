-- | Contains stuff to process bet rounds.
module Poker.Logic.Trading where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Operations with positions
-------------------------------------------------------------------------------

-- | Return first position depending on amount of players and street
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

-- | Return next position by clock arrow depending on player amount.
getNextPositionClockwise :: Position -> Int -> Position
getNextPositionClockwise pos amountOfPlayers = case amountOfPlayers of
  2 -> if (pos == BB)
        then SB
        else BB
  _ -> pos

-- | Return last position depending on amount of players and street.
getLastPosition :: Int -> Street -> Position
getLastPosition amountOfPlayers street = case amountOfPlayers of
  2 -> if (street == Preflop)
        then BB
        else SB
  _ -> BTN

-- | Return seat of player on given position.
--   Unsafe function for [] and lists without
--   player with given position.
getSeatOfPosition :: Position -> [Player] -> Seat
getSeatOfPosition pos players
    | (position $ head players) == pos = seat $ head players
    | otherwise = getSeatOfPosition pos $ tail players

-------------------------------------------------------------------------------
-- * Computations with player(-s)
-------------------------------------------------------------------------------

-- | Check if active player is skippable.
checkSkipForActivePlayer :: [Player] -> Bool
checkSkipForActivePlayer [] = False
checkSkipForActivePlayer players
  | active $ head players = case action . move $ head players of
      Bankrupted -> True
      Folded     -> True
      All_In_ed     -> True
      _          -> False
  | otherwise = checkSkipForActivePlayer $ tail players

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
      mv player /= Waiting && mv player /= Folded &&
      mv player /= All_In_ed && bt player /= bet)
  players)
  where
    mv p = action  $ move p
    bt p = betSize $ move p

-- | Return default move to proposed bet size when human didn't made any input.
autoHumanMove :: Player -> Int -> Move
autoHumanMove player bet
  | bet == 0          = Move Checked 0
  | bet == premadeBet = Move Checked bet
  | otherwise         = Move Folded premadeBet
  where
    premadeBet = betSize $ move player

-- | Calculate pot.
calculatePot :: [Player] -> Int
calculatePot players = foldl1 (+) (map
  (\player -> invested player)
  players)

-------------------------------------------------------------------------------
-- * Operations with player(-s)
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

-- | Write move to player on given position.
writeMove :: [Player] -> Position -> Move -> [Player]
writeMove players pos mv = map
  (\player -> case position player == pos of
      True  -> player { move = mv }
      False -> player)
  players    

-- | Decrease balance by bet, increase invested by bet, vanish move if can.
applyMoveResults :: [Player] -> [Player]
applyMoveResults players = map
  (\player -> player
    { balance  = balance  player - bet player
    , move     = case action $ move player of
        Bankrupted -> Move Bankrupted 0
        Folded     -> Move Folded 0
        All_In_ed     -> Move All_In_ed 0
        _          -> Move Waiting 0
    , active   = False
    , invested = invested player + bet player
    })
  players
  where
    bet p = betSize $ move p

-- | Find out hand results and reward winner(-s) or split pot if draw.
--   Also open all cards that should be shown at showdown. 
computeHandResults :: [Player] -> [Player]
computeHandResults players =
  if (countInHandPlayers players == 1)
    then map (\player -> case action $ move player of
                Folded -> player
                _      -> player { balance = balance player + snd tookFromEach })
      (fst tookFromEach)
    else players
  where
    maxInvested = maximum (map (\player -> invested player) players)
    tookFromEach = takePotFromPlayers players maxInvested

-- | Take from player amount of invested.  
takePotFromPlayer :: Player -> Int -> (Player, Int)
takePotFromPlayer player pot =
  if (invested player == 0)
    then (player, 0)
    else if (invested player <= pot)
      then (player { invested = 0 }, invested player)
      else (player { invested = invested player - pot }, pot)

-- | Take from each player amount of invested.
takePotFromPlayers :: [Player] -> Int -> ([Player], Int)
takePotFromPlayers players pot =
  (fst tookFromEach, foldl1 (+) $ snd tookFromEach)
  where
    tookFromEach = unzip $ map (\player -> takePotFromPlayer player pot) players

-- | Change player positions.
changePlayerPositions :: [Player] -> [Player]
changePlayerPositions players = map
    (\player -> player
      { position = getNextPositionClockwise (position player) (length players) })
    players

-- | Get possible actions for player depending on incoming bet.
--   Return only second two actions. Fold action is calculated independently.
getPossibleActions :: Player -> Int -> (ActionType, ActionType)
getPossibleActions player bet
    | bet == 0              = (Check,  Bet)
    | bet >= balance player = (All_In, All_In)
    | otherwise             = (Call,   Raise)

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Time to get response from AI player.
aiThinkTime :: Float
aiThinkTime = 2.0

-- | Time to get response from human player.
humanThinkTime :: Float
humanThinkTime = 10.0

-- | Time to display showdown results.
showdownTime :: Float
showdownTime = 3.0
