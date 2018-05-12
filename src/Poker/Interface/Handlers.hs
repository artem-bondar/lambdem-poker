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
    EventKey (MouseButton LeftButton) Down _ coords -> handleClick coords screen
    EventKey (MouseButton LeftButton) Up   _ _      ->
      screen { sliderData = (sliderData screen) { isSelected = False }}
    EventMotion (x, _) -> if (isSelected $ sliderData screen)
      then screen { sliderData = moveSliderBall x (blindSize screen) (sliderData screen) }
      else screen
    _ -> screen
  | otherwise = screen

-- | Handle mouse click.
handleClick :: (Float, Float) -> TableScreen -> TableScreen
handleClick hit screen
  | hittedButton      /= 0 = screen
  | hittedSmallButton /= 0 = screen
  | checkSliderHit hit = screen
    { sliderData = handleSliderClick (fst hit) (blindSize screen) $ sliderData screen }
  | otherwise = screen
  where
    hittedButton      = checkButtonHit hit buttonHitbox      False
    hittedSmallButton = checkButtonHit hit smallButtonHitbox True

-- | Handle button click depending on button number.
handleButtonClick :: Int -> TableScreen -> TableScreen
handleButtonClick btn screen = screen

-- | Handle small button click depending on button number.
handleSmallButtonClick :: Int -> TableScreen -> TableScreen
handleSmallButtonClick btn screen = screen

-- | Handle slider click depending on click horizontal position.
handleSliderClick :: Float -> Int -> Slider -> Slider
handleSliderClick x bb sliderr = sliderr
    { isSelected   = True
    , ballPosition = realX
    , currentValue = bet
    }
  where
    realX = x + fromIntegral (round sliderHalfWidth)
    bet = if (realX >= sliderHalfWidth * 2)
      then maxValue sliderr
      else minValue sliderr + floor (realX / stepSize sliderr) * bb
    sliderHalfWidth  = (fst sliderDimensions - 2 * fst sliderPadding) / 2

-- | Moves slider ball if possible.
moveSliderBall :: Float -> Int -> Slider -> Slider
moveSliderBall x bb sliderr = handleSliderClick newX bb sliderr
  where
    minX = fst $ fst sliderHitbox
    maxX = fst $ snd sliderHitbox
    newX = if (x < minX)
      then minX
      else if (x > maxX)
        then maxX
        else x

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Updates slider min and max values depending on player and incoming bet.
updateSlideData :: Slider -> Player -> Int -> Int -> Slider
updateSlideData sliderr player bet bb = sliderr
  { minValue     = minRaiseSize
  , maxValue     = balance player
  , currentValue = minRaiseSize
  , stepSize     = if (forRaise == 0)
      then sliderWidth
      else sliderWidth / (forRaise / fromIntegral bb)
  , ballPosition = 0
  , isSelected   = False
  }
  where
    minRaiseSize = if (bet == 0)
      then bb
      else if (bet < balance player `div` 2)
        then bet * 2
        else balance player
    forRaise = fromIntegral $ balance player - minRaiseSize
    sliderWidth  = fst sliderDimensions - 2 * fst sliderPadding

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
  ((fst sliderOffset - sliderBallAreaWidth,
    snd sliderOffset - sliderBallAreaHeight),
   (fst sliderOffset + sliderBallAreaWidth,
    snd sliderOffset + sliderBallAreaHeight))
  where
    sliderBallAreaWidth  = fromIntegral . round $ 
      (fst sliderDimensions - 2 * fst sliderPadding) / 2
    sliderBallAreaHeight = fromIntegral . round $
      (snd sliderDimensions - 2 * snd sliderPadding) / 2
