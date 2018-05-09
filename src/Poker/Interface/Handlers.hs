-- | Module that operates with input events.
module Poker.Interface.Handlers where

import Graphics.Gloss.Interface.Pure.Game

import Poker.Interface.Types
import Poker.Interface.Offsets

import Poker.Logic.Types   

import Debug.Trace

-------------------------------------------------------------------------------
-- * Handler functions
-------------------------------------------------------------------------------

-- | Operate with user input.
handleInput :: Event -> TableScreen -> TableScreen
handleInput event screen 
  | state screen == Waiting_User_Input = 
    case event of
      EventKey (MouseButton LeftButton) Down _ mouse -> trace (show mouse ++ " " ++
        (show $ checkButtonHit mouse)) screen
      _ -> screen
  | otherwise = screen

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return button number if clicked one.
checkButtonHit :: (Float, Float) -> Int
checkButtonHit hit = 
    if (hitY > minY && hitY < maxY && hitX > minX && hitX < (maxX + 2 * buttonOffset))
        then if (hitX < maxX)
            then 1
            else if (hitX < (maxX + buttonOffset))
                then 2
                else 3
        else 0
    where
        hitX = fst hit
        hitY = snd hit
        minX = fst $ fst buttonHitbox
        minY = snd $ fst buttonHitbox
        maxX = fst $ snd buttonHitbox
        maxY = snd $ snd buttonHitbox

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Height of button.
buttonHeight :: Float
buttonHeight = 56

-- | Bounding rectangle for first button.
buttonHitbox :: ((Float, Float), (Float, Float))
buttonHitbox = ((-buttonOffset * 1.5, buttonPositionOffset - 0.5 * buttonHeight),
                (-buttonOffset * 0.5, buttonPositionOffset + 0.5 * buttonHeight))

-- | Height of small button.
smallButtonHeight :: Float
smallButtonHeight = 24

-- | Bounding rectangle for first small button.
smallButtonHitbox :: ((Float, Float), (Float, Float))
smallButtonHitbox = ((fst smallButtonPositionOffset, snd smallButtonPositionOffset),
                     (fst smallButtonPositionOffset + smallButtonOffset,
                      snd smallButtonPositionOffset + smallButtonHeight))
