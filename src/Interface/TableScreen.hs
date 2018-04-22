module Interface.TableScreen where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game

-- | Launch main game screen.
launchTableScreen :: IO ()
launchTableScreen =  do
    tableScreen <- initTableScreen
    resolution <- getScreenSize
    play (display (screenPosition resolution)) bgColor fps tableScreen drawTableScreen handleTableScreen updateTableScreen
    where
      display = InWindow "Lambdem Poker" (screenSize)
      bgColor = white   -- цвет фона
      fps     = 30      -- кол-во кадров в секунду

screenSize :: (Int, Int)
screenSize = (960, 720)

screenPosition :: (Int, Int) -> (Int, Int)
screenPosition (w, h) 
    | w == 1920 = (480, height)
    | otherwise              = (10, height)
    where
      height
        | h == 1080 = 180
        | otherwise = 10


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
