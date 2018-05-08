-- | Contain functions for texture and sprites loading.
module Poker.Interface.Loader where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Data.Picture

import Poker.Interface.Types
import Poker.Logic.Types

-- | Load all table images from files.
loadedTableImages :: IO TableImages
loadedTableImages = do
  Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgTable      <- loadJuicyPNG "img/table.png"
  Just imgSeatBold   <- loadJuicyPNG "img/seatbold.png"
  Just imgBack       <- loadJuicyPNG "img/deck/back.png"
  imgsDeck           <- loadDeckLayout
  Just imgDealerChip <- loadJuicyPNG "img/chips/dealer.png"
  imgsChips          <- loadChipLayout
  return TableImages
    { background = imgBackground
    , table      = imgTable
    , seatBold   = imgSeatBold
    , deckLayout = DeckLayout
        { back  = imgBack
        , front = imgsDeck
        }
    , chipLayout = ChipLayout
        { dealerChip = imgDealerChip
        , stack      = imgsChips
        }
    }

-- | Load deck layout.
loadDeckLayout :: IO [Picture]
loadDeckLayout = do 
  maybePictures <- sequence loadedList
  return $ map unwrapMaybePicture maybePictures
  where
    loadedList = map
      (\x -> loadJuicyPNG $ "img/deck/" ++ x ++ ".png")
      allCardNames

-- | Load chip layout.
loadChipLayout :: IO [Chip]
loadChipLayout = do
  maybePictures <- sequence loadedList
  return $ wrapInChip allChipValues (map unwrapMaybePicture maybePictures)
  where
    wrapInChip values imgs =
      zipWith (\val img -> Chip val img) values imgs
    loadedList = map
      (\x -> loadJuicyPNG $ "img/chips/" ++ x ++ ".png")
      (map show allChipValues)

-- | Return `Picture` or blank.
unwrapMaybePicture :: Maybe Picture -> Picture
unwrapMaybePicture image = case image of
  Nothing  -> blank
  Just img -> img
