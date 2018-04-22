module Interface.TableScreen where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

-- | Launches main (table) game screen.
launchTableScreen :: IO ()
launchTableScreen =  do
    tableScreen <- initTableScreen
    resolution <- getScreenSize
    play (display (margins resolution)) bgColor fps tableScreen drawTableScreen handleTableScreen updateTableScreen
    where
      display = InWindow "Lambdem Poker" windowSize
      bgColor = white   -- background color
      fps     = 30      -- framerate

-- | Resolution.
windowSize :: (Int, Int)
windowSize = (960, 720)

-- | Margins to center window depending
-- on different display resolutions.
margins :: (Int, Int) -> (Int, Int)
margins (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

-- | Contains all data relative to table game screen.
data TableScreen = TableScreen
  { totalPlayers :: Int           -- amount of current players
  , playersData  :: [Player]      -- info about every player
  , handCount    :: Int           -- current hand number
  , screenImages :: TableImages   -- all images
  }

-- | Contains all personal player data.
data Player = Player
  { name     :: String
  , balance  :: Int
  , position :: Position
  }

-- | Poker positions
data Position = UTG1 | UTG2 | MP1 | MP2 | HJ | CO | BTN | SB | BB

-- | All images relative to table game screen.
data Images = Images
 { background :: Picture
 , table      :: Picture
 }

initTableScreen :: IO TableScreen
initTableScreen = pure TableScreen {totalPlayers = 2}

drawTableScreen :: TableScreen -> Picture
drawTableScreen _ = loadJuicyPNG "images/background.png"

handleTableScreen :: Event -> TableScreen -> TableScreen
handleTableScreen _ = id

updateTableScreen :: Float -> TableScreen -> TableScreen
updateTableScreen _ = id
