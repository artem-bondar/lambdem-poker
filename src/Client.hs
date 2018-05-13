-- | Core module that handles all major game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen)

import Poker.Interface.Handlers
import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types

import Poker.Logic.Dealer
import Poker.Logic.Trading
import Poker.Logic.Types

--import Debug.Trace

-------------------------------------------------------------------------------
-- * Game launch related functions
-------------------------------------------------------------------------------

-- | Launches game screen.
launchGame :: IO ()
launchGame =  do
  generator   <- getStdGen
  tableScreen <- initTableScreen generator
  resolution  <- getScreenSize
  play (display $ getMarginsFrom resolution)
    backgroundColor fps tableScreen
    drawTableScreen handleInput updateGame
  where
    display         = InWindow "Lambdem Poker" windowSize
    backgroundColor = white
    fps             = 25

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
    [Player Human " Hero"    1500 SB Bottom Nothing False False 0 (Move Waiting 0) 0,
     Player Human "Opponent" 1500 BB Top    Nothing True  False 0 (Move Waiting 0) 0]
  , street     = Preflop
  , handCount  = 1
  , dealer     = Bottom
  , blindSize  = 30
  , sliderData = Slider 0 0 0 0 0 False
  , board      = []
  , randomizer = generator
  , deck       = Deck 0 []
  , images     = imgs
  }

-------------------------------------------------------------------------------
-- * Game maintance related functions
-------------------------------------------------------------------------------

-- | Update game parameters depending on game state.
updateGame :: Float -> TableScreen -> TableScreen
updateGame timePassed screen 
  | state screen == Dealing_Hand = 
    if (timer screen < dealTime)
      then screen { timer = timer screen + timePassed }
      else screen 
        { state      = Posting_Blinds
        , timer      = 0
        , players    = hideHands $ fst dealResult
        , street     = Preflop
        , dealer     = getSeatOfPosition buttonPosition (players screen)
        , randomizer = fst $ snd dealResult
        , deck       = snd $ snd dealResult
        }
  | state screen == Posting_Blinds =
    if (timer screen < postTime)
      then screen { timer = timer screen + timePassed }
      else screen
        { state   = Start_Round
        , players = takeBlinds (players screen) (blindSize screen)
        }
  | state screen == Start_Round =
    if (street screen == Showdown)
      then screen
      { state = Finish_Hand
      , timer = 0
      }
      else screen 
        { state      = Bet_Round
        , players    = toggleNewActivePlayer (players screen) firstPosition
        , board      = fst boardDealResult
        , randomizer = fst $ snd boardDealResult
        , deck       = snd $ snd boardDealResult
        }
  | state screen == Bet_Round =
    if (checkSkipForActivePlayer $ players screen)
      then screen { state = Next_Move }
      else screen
        { state = case activePlayerType of
            Human -> Waiting_User_Input
            AI    -> AI_Thinking
        , timer      = 0
        , sliderData = updateSlideData (sliderData screen)
            activePlayer maxBet (blindSize screen)
        }
  | state screen == Show_Click = 
    if (timer screen < clickTime)
      then screen { timer = timer screen + timePassed }
      else screen
      { state   = Next_Move
      , players = writeMove (players screen) activePlayerPosition $ 
          getMoveFromButtonPressed (pressed activePlayer) possibleActions
          maxBet (currentValue $ sliderData screen) activePlayer }
  | state screen == Waiting_User_Input =
    if (timer screen < humanThinkTime)
      then screen { timer = timer screen + timePassed }
      else screen
        { state   = Next_Move
        , players = writeMove (players screen) activePlayerPosition
            (autoHumanMove activePlayer maxBet)
        }
  | state screen == AI_Thinking = 
    if (timer screen < aiThinkTime)
      then screen { timer = timer screen + timePassed }
      else screen
        { state   = Next_Move
        , players = writeMove (players screen) activePlayerPosition
            (autoHumanMove activePlayer maxBet)
        }
  | state screen == Next_Move =
    if (countInHandPlayers (players screen) == 1)
      then screen
        { state   = Finish_Hand
        , timer   = 0
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
  | state screen == Finish_Hand =
    if (timer screen < showdownTime)
      then screen { timer = timer screen + timePassed }
      else screen
        { state     = Dealing_Hand
        , timer     = 0
        , players   = changePlayerPositions $ computeHandResults
            (players screen) (board screen)
        , handCount = succ $ handCount screen
        , board     = []
        }
  | otherwise = screen
  where
    maxBet               = countMaxBet $ players screen
    activePlayer         = getActivePlayer $ players screen
    activePlayerType     = control  activePlayer
    activePlayerPosition = position activePlayer
    possibleActions      = getPossibleActions activePlayer maxBet
    dealResult      = dealPlayers (players screen) (randomizer screen) createDeck
    boardDealResult = dealBoard (randomizer screen) (deck screen) (board screen) (street screen)
    firstPosition   = getFirstPosition (length $ players screen) (street screen)
    nextPosition    = getNextPositon   (length $ players screen) (street screen)
                                       activePlayerPosition
    lastPosition    = getLastPosition  (length $ players screen) (street screen)
    buttonPosition  = if (length (players screen) == 2)
                      then SB
                      else BTN
    