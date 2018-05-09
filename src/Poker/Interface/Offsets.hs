-- | Contain offsets for images.
module Poker.Interface.Offsets where

import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Offsets
-------------------------------------------------------------------------------

-- | Return offset for cards depending on seat.
getHandOffset :: Seat -> (Float, Float)
getHandOffset s = case s of
  Bottom     -> (-32, -87)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (-32, 261)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0)

-- | Return offset for seatbold depending on seat.
getSeatBoldOffset :: Seat -> (Float, Float)
getSeatBoldOffset s = case s of
  Bottom     -> (0, -120)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (0, 228)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0) 

-- | Return offset for dealer chip depending on seat.
getDealerChipOffset :: Seat -> (Float, Float)
getDealerChipOffset s = case s of
  Bottom     -> (100, -60)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (-110, 160)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0) 

-- | Return offset for player name depending on seat.
getTextNameOffset :: Seat -> (Float, Float)
getTextNameOffset s = case s of
  Bottom     -> (-32, -115)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (-32, 233)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0) 

-- | Return offset for player balance depending on seat.
getBalanceOffset :: Seat -> (Float, Float)
getBalanceOffset s = case s of
  Bottom     -> (-16, -138)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (-16, 210)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0)

-- | Return chip offset for player bet depending on seat
--   and amount of chip columns.
getChipsOffset :: Seat -> Int -> (Float, Float)
getChipsOffset s columns = case s of
  Bottom     -> (fromIntegral (columns - 1) * (-chipColumnOffset / 2), -22)
  Left_Down  -> (0, 0)
  Left_Up    -> (0, 0)
  Top        -> (fromIntegral (columns - 1) * (-chipColumnOffset / 2), 172)
  Right_Up   -> (0, 0)
  Right_Down -> (0, 0)

-- | Return pot offset depending on amount of columns. 
getPotOffset :: Int -> (Float, Float)
getPotOffset columns = 
  (fromIntegral (columns - 1) * (-chipColumnOffset / 2), 125)    

-- | Return buttons offset depending on possible middle action.
getButtonsOffset :: ActionType -> (Float, Float)
getButtonsOffset act = case act of
  Check -> (-buttonOffset / 2,   buttonPositionOffset)
  _     -> (-buttonOffset,       buttonPositionOffset)

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Right card offset.
cardOffset :: Float
cardOffset = 65

-- | Horizontal offset between chip columns.
chipColumnOffset :: Float
chipColumnOffset = 38

-- | Vertical offset between chips in one column.
chipTowerOffset :: Float
chipTowerOffset = 5

-- | Offset for cards that are displayed on table.
cardsOnTableOffset :: (Float, Float)
cardsOnTableOffset = (-140, 57)

-- | Offset between cards that are displayed on table.
cardsOnTableRowOffset :: Float
cardsOnTableRowOffset = 70

-- | Offset for button.
buttonOffset :: Float
buttonOffset = 134

-- | Vertical offset for buttons.
buttonPositionOffset :: Float
buttonPositionOffset = -300

-- | Offset for small button.
smallButtonOffset :: Float
smallButtonOffset = 58

-- | Offset for small buttons.
smallButtonPositionOffset :: (Float, Float)
smallButtonPositionOffset =
  (-5, -220)

-- | Offset for slider.
sliderOffset :: (Float, Float)
sliderOffset = (0, -250)

-- | Offset for bet window.
betWindowOffset :: (Float, Float)
betWindowOffset = (buttonOffset * 1.5 - 33, -250)

-- | Offset for test in bet window.
betWindowTextOffset :: (Float, Float)
betWindowTextOffset = (-20, -6)
