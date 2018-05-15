-- | Core module that handles all major game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen)

import Poker.AI.Engine
import Poker.AI.PlayStyles
import Poker.AI.Types

import Poker.Interface.Handlers
import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types

import Poker.Logic.Dealer
import Poker.Logic.Trading
import Poker.Logic.Types.Cards
import Poker.Logic.Types.Game

-------------------------------------------------------------------------------
-- * Game launch related functions
-------------------------------------------------------------------------------

-- | Launches game screen.
launchGame :: IO ()
launchGame =  do
  generator   <- getStdGen
  playstyle   <- parsePlaystyle getArgs
  tableScreen <- initTableScreen generator playstyle
  resolution  <- getScreenSize
  play (display $ getMarginsFrom resolution)
    backgroundColor fps tableScreen
    drawTableScreen handleInput updateGame
  where
    display         = InWindow "Lambdem Poker" windowSize
    backgroundColor = white
    fps             = 25

-- | Parse command line arguments.
parsePlaystyle :: IO [String] -> IO PlayStyleType
parsePlaystyle arguments = do
  args <- arguments
  if (length args == 1)
    then case head args of
      "telephone" -> return Telephone
      "passive"   -> return Passive
      "loose"     -> return Loose
      "tight"     -> return Tight
      "aggresive" -> return Aggresive
      _           -> return Random
    else return Random

-- | Initialization of table screen.
--   All images are loaded and all player data is set.
initTableScreen :: StdGen -> PlayStyleType -> IO TableScreen
initTableScreen generator playstyle = createTableScreenWith generator playstyle <$> loadedTableImages

-- | Create new table screen made of images and set default parameters.
createTableScreenWith :: StdGen -> PlayStyleType -> TableImages -> TableScreen
createTableScreenWith generator playstyle imgs = TableScreen
  { state      = Dealing_Hand
  , timer      = 0.0
  , players    =
    [Player Human " Hero"    3000 SB Bottom Nothing
     False False 0 (Move Waiting 0) 0 Nothing,
     Player AI    "Opponent" 3000 BB Top    Nothing
     True  False 0 (Move Waiting 0) 0
     (Just (getAIPlayer playstyle generator))]
  , hero       = " Hero"
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
  | state screen == Start_Hand =
    if (checkGameEnd $ players screen)
      then screen { state = Finish_Game }
      else screen { state = Dealing_Hand }
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
      { state   = Finish_Hand
      , timer   = 0
      , players = openHands $ players screen
      }
      else screen 
        { state      = Bet_Round
        , players    = toggleNewActivePlayer (players screen) firstPosition
        , board      = fst boardDealResult
        , randomizer = fst $ snd boardDealResult
        , deck       = snd $ snd boardDealResult
        }
  | state screen == Bet_Round =
    if (checkSkipForActivePlayer activePlayer maxBet $
        countCanMovePlayers (players screen))
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
        , players = writeAIDataChange (writeMove (players screen) activePlayerPosition
            (fst aiCalculationResults)) activePlayerPosition (snd aiCalculationResults)
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
        { state     = Start_Hand
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
    aiCalculationResults = calculateAIMove (updateAIData activePlayer $ board screen)
      maxBet (blindSize screen) (calculatePot $ players screen) (street screen)
    