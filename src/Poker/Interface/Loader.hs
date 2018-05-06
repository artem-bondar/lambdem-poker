-- | Contains functions for texture and sprites loading.
module Poker.Interface.Loader where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Data.Picture

import Poker.Interface.Types
import Poker.Logic.Types

-- | Loades images from files.
loadedImages :: IO TableImages
loadedImages = do
  Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgTable      <- loadJuicyPNG "img/table.png"
  Just imgBack       <- loadJuicyPNG "img/deck/back.png"
  imgsDeck           <- loadDeckLayout
  return TableImages
    { background = imgBackground
    , table      = imgTable
    , deckLayout = DeckLayout
      { back = imgBack
      , deck = imgsDeck
      }
    }

loadDeckLayout :: IO [Picture]
loadDeckLayout = sequence (uwrapMaybes <$> loadedList)
  where
    uwrapMaybes x = do
      u <- x
      case u of
        Nothing -> return blank
        Just v  -> return v
    loadedList = map
                (\x -> loadJuicyPNG $ "img/deck/" ++ x ++ ".png")
                allCardNames
