-- | Responsible for drawing all content on the screen.
module Poker.Interface.Renderer where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

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
        drawHand False (hand $ players screen !! 0) (deckLayout $ images screen)]

-- | Draw player seatbolds.
drawSeatBold :: Position -> Picture -> Picture
drawSeatBold position img = blank

-- | Draw cards.
drawHand :: Bool -> Maybe (Card, Card) -> DeckLayout -> Picture
drawHand hide hnd layout = case hnd of
    Nothing -> blank
    Just h  -> case hide of
        True  -> pictures [back layout, uncurry translate cardOffset (back layout)]
        False -> pictures [front layout !! (fromEnum $ fst h),
            uncurry translate cardOffset (front layout !! (fromEnum $ snd h))]

-- | Apply offset for player hand depending on seat.
--drawPlayerHand :: Position -> Picture -> Picture

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Calculate margins for window depending on display resolutions.
getMarginsFrom :: (Int, Int) -> (Int, Int)
getMarginsFrom (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Resolution.
windowSize :: (Int, Int)
windowSize = (960, 720)

-- | Right card offset.
cardOffset :: (Float, Float)
cardOffset = (65, 0)
