-- | Contains functions for texture and sprites loading.
module Poker.Interface.Loader where

import Poker.Interface.Types
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- | Loades images from files.
loadedImages :: IO TableImages
loadedImages = do
  Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgTable      <- loadJuicyPNG "img/table.png"
  return Images
    { background = imgBackground
    , table      = imgTable
    }