module Client where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

-- | Launches main (table) game screen.
launchTableScreen :: Images -> IO ()
launchTableScreen images =  do
    tableScreen <- initTableScreen
    resolution  <- getScreenSize
    test <- return (display (margins resolution))
    play test color fps tableScreen (drawTableScreen images) handleTableScreen updateTableScreen
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

initTableScreen :: IO TableScreen
initTableScreen = pure TableScreen {totalPlayers = 2}

drawTableScreen :: Images -> TableScreen -> Picture
drawTableScreen images sprites = pictures [background images, table images]

handleTableScreen :: Event -> TableScreen -> TableScreen
handleTableScreen _ = id

updateTableScreen :: Float -> TableScreen -> TableScreen
updateTableScreen _ = id
