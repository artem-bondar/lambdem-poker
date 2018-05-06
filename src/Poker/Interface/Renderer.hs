-- | Responsible for drawing all content on the screen.
module Poker.Interface.Renderer where

import Graphics.Gloss.Data.Picture
--import Graphics.Gloss.Data.Color

import Poker.Interface.Types
import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Render functions
-------------------------------------------------------------------------------

-- | Draw tablescreen.
drawTableScreen :: TableScreen -> Picture
drawTableScreen screen 
    | state screen == Dealing_Hand = pictures
        [background $ images screen,  table $ images screen]
    | otherwise = pictures [background $ images screen,  table $ images screen,
        drawPlayerHand (players screen !! 0) (deckLayout $ images screen),
        drawPlayerSeatBold (players screen !! 0) (seatBold $ images screen)]

-- | Draw player seatbold.
drawPlayerSeatBold :: Player -> Picture -> Picture
drawPlayerSeatBold player img =
    uncurry translate (getSeatBoldOffset $ seat player) img

-- | Draw cards.
drawHand :: Bool -> Maybe (Card, Card) -> DeckLayout -> Picture
drawHand hide hnd layout = case hnd of
    Nothing -> blank
    Just h  -> case hide of
        True  -> pictures [back layout, uncurry translate cardOffset (back layout)]
        False -> pictures [front layout !! (fromEnum $ fst h),
            uncurry translate cardOffset (front layout !! (fromEnum $ snd h))]

-- | Apply offset for player hand depending on seat.
drawPlayerHand :: Player -> DeckLayout -> Picture
drawPlayerHand player layout = 
    uncurry translate (getHandOffset $ seat player) img
    where
        img = drawHand (hideHand player) (hand player) layout
-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Calculate margins for window depending on display resolutions.
getMarginsFrom :: (Int, Int) -> (Int, Int)
getMarginsFrom (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

-- | Return offset for cards depending on seat.
getHandOffset :: Seat -> (Float, Float)
getHandOffset s = case s of
    Bottom     -> (-32, -57)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (-32, 239)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0)

-- | Return offset for seatbold depending on seat.
getSeatBoldOffset :: Seat -> (Float, Float)
getSeatBoldOffset s = case s of
    Bottom     -> (0, -90)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (0, 205)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0) 

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Resolution.
windowSize :: (Int, Int)
windowSize = (960, 720)

-- | Right card offset.
cardOffset :: (Float, Float)
cardOffset = (65, 0)
