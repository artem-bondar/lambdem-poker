module Interface.TableScreen where

import Graphics.Gloss

launchTableScreen :: IO ()
launchTableScreen =  display (InWindow "Game window" (200, 200) (10, 10)) white (Circle 80)
