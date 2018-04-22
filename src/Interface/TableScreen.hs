module Interface.TableScreen where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

-- | Launch main game screen.
launchTableScreen :: IO ()
launchTableScreen =  do
    tableScreen <- initTableScreen
    resolution <- getScreenSize
    play (display (margins resolution)) bgColor fps tableScreen drawTableScreen handleTableScreen updateTableScreen
    where
      display = InWindow "Lambdem Poker" windowSize
      bgColor = white   -- background color
      fps     = 30      -- framerate

-- | Resolution
windowSize :: (Int, Int)
windowSize = (960, 720)

-- | Margins to center window depending
-- on different display resolutions
margins :: (Int, Int) -> (Int, Int)
margins (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

data TableScreen = TableScreen
  { var :: Int
  }

initTableScreen :: IO TableScreen
initTableScreen = pure TableScreen {var = 1}

drawTableScreen :: TableScreen -> Picture
drawTableScreen _ = text "some text"

handleTableScreen :: Event -> TableScreen -> TableScreen
handleTableScreen _ = id

updateTableScreen :: Float -> TableScreen -> TableScreen
updateTableScreen _ = id
