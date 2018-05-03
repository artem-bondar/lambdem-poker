-- | Core module that handles all game processes.
module Client where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.Pure.Game

import Poker.Interface.Loader
import Poker.Interface.Renderer
import Poker.Interface.Types
import Poker.Logic.Types

-- | Launches main (table) game screen.
launchGame :: IO ()
launchGame =  do
    tableScreen <- initTableScreen
    resolution  <- getScreenSize
    play (display $ getMarginsFrom resolution)
      backgroundColor fps tableScreen
      drawTableScreen handleInput updateGame
    where
      display           = InWindow "Lambdem Poker" windowSize
      backgroundColor   = white
      fps               = 30

-- | Initialization of table screen.
--   All images are loaded and all player data is set.
initTableScreen :: IO TableScreen
initTableScreen = createTableScreenWith <$> loadedImages

-- | Create new table screen made of images and set default parameters.
createTableScreenWith :: TableImages -> TableScreen
createTableScreenWith imgs = TableScreen
  {
  totalPlayers  = 2
  , playersData = [Player "1" 1500 SB, Player "2" 1500 BB]
  , handCount   = 1
  , images      = imgs
  }

-- | Operate with user input.
handleInput :: Event -> TableScreen -> TableScreen
handleInput _ = id

-- | Update game status. Is used to operate with timebank. 
updateGame :: Float -> TableScreen -> TableScreen
updateGame _ = id
