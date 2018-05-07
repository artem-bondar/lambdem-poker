-- | Core module that handles all game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen)

import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types

import Poker.Logic.Dealer
import Poker.Logic.Trading
import Poker.Logic.Types

import Debug.Trace

-- | Launches main (table) game screen.
launchGame :: IO ()
launchGame =  do
    generator   <- getStdGen
    tableScreen <- initTableScreen generator
    resolution  <- getScreenSize
    play (display $ getMarginsFrom resolution)
      backgroundColor fps tableScreen
      drawTableScreen handleInput updateGame
    where
      display           = InWindow "Lambdem Poker" windowSize
      backgroundColor   = white
      fps               = 25

-- | Initialization of table screen.
--   All images are loaded and all player data is set.
initTableScreen :: StdGen -> IO TableScreen
initTableScreen generator = createTableScreenWith generator <$> loadedTableImages

-- | Create new table screen made of images and set default parameters.
createTableScreenWith :: StdGen -> TableImages -> TableScreen
createTableScreenWith generator imgs = TableScreen
    { state      = Dealing_Hand
    , timer      = 0.0
    , players    =
        [Player Human " Hero"    1500 SB Bottom Nothing False False (Move No_Action 0) 0,
         Player Human "Opponent" 1500 BB Top    Nothing True  False (Move No_Action 0) 0]
    , street     = Preflop
    , handCount  = 1
    , dealer     = Bottom
    , blindSize  = 30
    , board      = []
    , randomizer = generator
    , deck       = Deck 0 []
    , images     = imgs
    }

-- | Operate with user input.
handleInput :: Event -> TableScreen -> TableScreen
handleInput _ = id

-- | Update game parameters depending on game state.
updateGame :: Float -> TableScreen -> TableScreen
updateGame timePassed screen 
    | state screen == Dealing_Hand = 
        if (timer screen < dealTime)
            then screen { timer = timer screen + timePassed }
            else screen 
                { state      = Posting_Blinds
                , timer      = 0
                , players    = fst dealResult
                , street     = Preflop
                , randomizer = fst $ snd dealResult
                , deck       = snd $ snd dealResult
                }
    | state screen == Posting_Blinds =
        if (timer screen < postTime)
            then screen { timer = timer screen + timePassed }
            else screen
                { state    = Start_Round
                , players  = takeBlinds (players screen) (blindSize screen)
                }
    | state screen == Start_Round =
        if (street screen == Showdown)
            then screen { state = Finish_Hand }
            else screen 
                { state   = Bet_Round
                , players = toggleNewActivePlayer (players screen) firstPosition
                }
    | state screen == Bet_Round =
        if (checkSkipForActivePlayer $ players screen)
            then screen { state = Next_Move }
            else screen
                { state = case activePlayerType of
                    Human -> Waiting_User_Input
                    AI    -> AI_Thinking
                , timer    = 0
                }
    | state screen == Waiting_User_Input =
        if (timer screen < humanThinkTime)
            then screen { timer = timer screen + timePassed }
            else screen
                { state    = Bet_Round
                , players  = writeMove (players screen) activePlayerPosition
                    (autoHumanMove (getActivePlayer $ players screen) maxBet)
                }
    | state screen == AI_Thinking = 
        if (timer screen < aiThinkTime)
            then screen { timer = timer screen + timePassed }
            else screen
                { state    = Bet_Round
                , players  = writeMove (players screen) activePlayerPosition
                    (autoHumanMove (getActivePlayer $ players screen) maxBet)
                }
    | state screen == Next_Move =
        if (countInHandPlayers (players screen) == 1)
            then screen
                { state   = Finish_Hand
                , players = applyMoveResults (players screen)
                }
            else if (activePlayerPosition == lastPosition)
                then if (checkReTrade (players screen) maxBet)
                    then screen { state = Start_Round }
                    else screen
                        { state   = Start_Round
                        , players = applyMoveResults (players screen)
                        , street  = succ $ street screen
                        }
                else screen
                    { state   = Bet_Round
                    , players = toggleNewActivePlayer (players screen) nextPosition
                    }
    | state screen == Finish_Hand = screen
    | otherwise = screen
    where
        activePlayerType     = control  . getActivePlayer $ players screen
        activePlayerPosition = position . getActivePlayer $ players screen
        dealResult    = dealPlayers (players screen) (randomizer screen) createDeck
        firstPosition = getFirstPosition (length $ players screen) (street screen)
        nextPosition  = getNextPositon   (length $ players screen) (street screen)
                                          activePlayerPosition
        lastPosition  = getLastPosition  (length $ players screen) (street screen)
        maxBet        = countMaxBet $ players screen
      