-- | Contain functions for texture and sprites loading.
module Poker.Interface.Loader where

import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Data.Picture

import Poker.Interface.Types
import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Load functions
-------------------------------------------------------------------------------

-- | Load all table images from files.
loadedTableImages :: IO TableImages
loadedTableImages = do
  Just imgBackground     <- loadJuicyPNG "img/background.png"
  Just imgWin            <- loadJuicyPNG "img/text/win.png"
  Just imgLoss           <- loadJuicyPNG "img/text/loss.png"
  Just imgTable          <- loadJuicyPNG "img/table.png"
  Just imgSeatBold       <- loadJuicyPNG "img/seatbold.png"
  Just imgSeatBoldActive <- loadJuicyPNG "img/seatbold active.png"
  Just imgSlider         <- loadJuicyPNG "img/slider.png"
  Just imgSliderBall     <- loadJuicyPNG "img/slider ball.png"
  Just imgButton         <- loadJuicyPNG "img/button.png"
  Just imgButtonClicked  <- loadJuicyPNG "img/button clicked.png"
  imgsButtonTexts        <- loadButtonTexts
  Just imgSmallButton    <- loadJuicyPNG "img/small button.png"
  imgsSmallButtonTexts   <- loadSmallButtonTexts
  Just imgBetWindow      <- loadJuicyPNG "img/bet window.png"
  Just imgBack           <- loadJuicyPNG "img/deck/back.png"
  imgsDeck               <- loadDeckLayout
  Just imgDealerChip     <- loadJuicyPNG "img/chips/dealer.png"
  imgsChips              <- loadChipLayout
  return TableImages
    { background       = imgBackground
    , win              = imgWin
    , loss             = imgLoss
    , table            = imgTable
    , seatBold         = imgSeatBold
    , seatBoldActive   = imgSeatBoldActive
    , slider           = imgSlider
    , sliderBall       = imgSliderBall
    , button           = imgButton
    , buttonClicked    = imgButtonClicked
    , buttonTexts      = imgsButtonTexts
    , smallButton      = imgSmallButton
    , smallButtonTexts = imgsSmallButtonTexts
    , betWindow        = imgBetWindow
    , deckLayout       = DeckLayout
        { back  = imgBack
        , front = imgsDeck
        }
    , chipLayout       = ChipLayout
        { dealerChip = imgDealerChip
        , stack      = imgsChips
        }
    }

-- | Load small button texts.
loadSmallButtonTexts :: IO [SmallButtonText]
loadSmallButtonTexts = do
  maybePictures <- sequence loadedList
  return $ wrapInSmallButtonText allBetSizings
    (map unwrapMaybePicture maybePictures)
  where
    wrapInSmallButtonText values imgs =
      zipWith (\val img -> SmallButtonText val img) values imgs
    loadedList = map
      (\x -> loadJuicyPNG $ "img/text/" ++ show x ++ ".png")
      allBetSizings

-- | Load button texts.
loadButtonTexts :: IO [ButtonText]
loadButtonTexts = do
  maybePictures <- sequence loadedList
  return $ wrapInButtonText [minBound..maxBound :: ActionType]
    (map unwrapMaybePicture maybePictures)
  where
    wrapInButtonText values imgs =
      zipWith (\val img -> ButtonText val img) values imgs
    loadedList = map
      (\x -> loadJuicyPNG $ "img/text/" ++ x ++ ".png")
      allActionNames

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

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return `Picture` or blank.
unwrapMaybePicture :: Maybe Picture -> Picture
unwrapMaybePicture image = case image of
  Nothing  -> blank
  Just img -> img
