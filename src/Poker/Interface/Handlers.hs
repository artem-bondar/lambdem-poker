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
        (show $ checkButtonHit mouse smallButtonHitbox True)) screen
      _ -> screen
  | otherwise = screen

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return button number if clicked one depending on first button rectangle.
checkButtonHit :: (Float, Float) -> ((Float, Float), (Float, Float)) -> Bool -> Int
checkButtonHit hit rectangle isSmall = 
    if (hitY > minY && hitY < maxY && hitX > minX && hitX < (maxX + 3 * offset))
        then if (hitX < maxX)
            then 1
            else if (hitX < (maxX + offset))
                then 2
                else if (hitX < (maxX + 2 * offset))
                    then 3
                    else case isSmall of
                        True  -> 4
                        False -> 0
        else 0
    where
        offset = case isSmall of
            True  -> smallButtonOffset
            False -> buttonOffset
        hitX   = fst hit
        hitY   = snd hit
        minX   = fst $ fst rectangle
        minY   = snd $ fst rectangle
        maxX   = fst $ snd rectangle
        maxY   = snd $ snd rectangle

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
smallButtonHitbox = 
    ((fst smallButtonPositionOffset - 0.5 * smallButtonOffset,
      snd smallButtonPositionOffset - 0.5 * smallButtonHeight),
     (fst smallButtonPositionOffset + 0.5 * smallButtonOffset,
      snd smallButtonPositionOffset + 0.5 * smallButtonHeight))
