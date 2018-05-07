-- | Core module that handles all game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen)

import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types
import Poker.Logic.Types
import Poker.Logic.Dealer

--import Debug.Trace

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
        [Player Human " Hero"    1500 SB Bottom Nothing False (Move No_Action 0) False,
         Player Human "Opponent" 1500 BB Top    Nothing True  (Move No_Action 0) False]
    , street     = Preflop
    , inHand     = 0
    , retrade    = False
    , handCount  = 1
    , dealer     = Bottom
    , blindSize  = 30
    , pot        = Nothing
    , sidePot    = Nothing
    , flop       = Nothing
    , turn       = Nothing
    , river      = Nothing
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
            then screen
                { timer = timer screen + timePassed }
            else screen 
                { state      = Posting_Blinds
                , timer      = 0
                , players    = fst dealResult
                , street     = Preflop
                , inHand     = length $ players screen
                , randomizer = fst $ snd dealResult
                , deck       = snd $ snd dealResult
                }
    | state screen == Posting_Blinds =
        if (timer screen < postTime)
            then screen
                { timer = timer screen + timePassed }
            else screen
                { state   = Bet_Round
                , timer   = 0
                , players = takeBlinds (players screen) (blindSize screen)
                }
    | state screen == Bet_Round = screen
    | otherwise = screen
    where
      dealResult = dealPlayers (players screen)
        (randomizer screen) createDeck
      