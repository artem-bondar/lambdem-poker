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
      (show $ checkSliderHit mouse)) screen
    --EventMotion (x, _) -> trace (show x) screen
    _ -> screen
  | otherwise = screen

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Return clicked button number depending on first button bounding rectangle.
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

-- | Return if clicked slider area.
checkSliderHit :: (Float, Float) -> Bool
checkSliderHit hit =
  hitY > minY && hitY < maxY && hitX > minX && hitX < maxX
  where
    hitX = fst hit
    hitY = snd hit
    minX = fst $ fst sliderHitbox
    minY = snd $ fst sliderHitbox
    maxX = fst $ snd sliderHitbox
    maxY = snd $ snd sliderHitbox

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

-- | Slider inside padding to slider ball area.
sliderPadding :: (Float, Float)
sliderPadding = (8, 4)

-- | Slider dimensions.
sliderDimensions :: (Float, Float)
sliderDimensions = (263, 24)

-- | Slider horizontal bounds.
sliderHitbox :: ((Float, Float), (Float, Float))
sliderHitbox =
  ((fst sliderOffset - 0.5 * sliderBallAreaWidth,
    snd sliderOffset - 0.5 * sliderBallAreaHeight),
   (fst sliderOffset + 0.5 * sliderBallAreaWidth,
    snd sliderOffset + 0.5 * sliderBallAreaHeight))
  where
    sliderBallAreaWidth  = fst sliderDimensions - 2 * fst sliderPadding
    sliderBallAreaHeight = snd sliderDimensions - 2 * snd sliderPadding
