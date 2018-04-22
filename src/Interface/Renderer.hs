module Interface.Renderer where

import Interface.TableScreen
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- | Loades images from files.
loadImages :: IO Images
loadImages = do
  Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgTable      <- loadJuicyPNG "img/table.png"
  return Images
    { background = imgBackground
    , table      = imgTable
    }