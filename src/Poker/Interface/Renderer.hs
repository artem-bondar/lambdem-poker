-- | Responsible for drawing all content on the screen.
module Poker.Interface.Renderer where

import Graphics.Gloss.Data.Picture

import Poker.Interface.Types

-------------------------------------------------------------------------------
-- * Render functions
-------------------------------------------------------------------------------

-- | Draw tablescreen.
drawTableScreen :: TableScreen -> Picture
drawTableScreen screen = pictures [background $ images screen, table $ images screen]

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
