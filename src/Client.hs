module Client where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

import Poker.Interface.Renderer
import Poker.Interface.Types
import Poker.Logic.Types

-- | Launches main (table) game screen.
launchGame :: IO ()
launchGame =  do
    tableScreen <- initTableScreen
    resolution  <- getScreenSize
    test <- return (display (margins resolution))
    play test color fps tableScreen drawTableScreen handleInput updateGame
    where
      display = InWindow "Lambdem Poker" windowSize
      color   = white -- background color
      fps     = 30    -- framerate

-- | Resolution.
windowSize :: (Int, Int)
windowSize = (960, 720)

-- | Margins to center window depending
-- on different display resolutions.
margins :: (Int, Int) -> (Int, Int)
margins (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

-- | Initialization of table screen.
--   All images are loaded and
--   all player data is set
initTableScreen :: IO TableScreen
initTableScreen = createTableScreenWith <$> loadedImages

-- | Create new table screen made of
--   images and set default parameters
createTableScreenWith :: TableImages -> TableScreen
createTableScreenWith imgs = TableScreen
  {
  totalPlayers  = 2
  , playersData = [Player "1" 1500 SB, Player "2" 1500 BB]
  , handCount   = 1
  , images      = imgs
  }

drawTableScreen :: TableScreen -> Picture
drawTableScreen screen = pictures [background $ images screen, table $ images screen]

handleInput :: Event -> TableScreen -> TableScreen
handleInput _ = id

updateGame :: Float -> TableScreen -> TableScreen
updateGame _ = id
