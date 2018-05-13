-- | Responsible for drawing all content on the screen.
module Poker.Interface.Renderer where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Poker.Interface.Handlers (sliderDimensions, sliderPadding)
import Poker.Interface.Types
import Poker.Interface.Offsets

import Poker.Logic.Trading
import Poker.Logic.Types

-------------------------------------------------------------------------------
-- * Render functions
-------------------------------------------------------------------------------

-- | Draw tablescreen.
drawTableScreen :: TableScreen -> Picture
drawTableScreen screen 
  | state screen == Dealing_Hand = pictures ([tableWithDealerChip] ++
      map (\p -> playerOnSeatBold p) (players screen))
  | state screen == Waiting_User_Input ||
    state screen == Show_Click = pictures [tableWithDealerChip, potWithBoard, playersHands, playersBets,
      drawButtons possibleActions (button $ images screen, buttonClicked $ images screen)
      (buttonTexts $ images screen) (pressed activePlayer), sliderImage, smallButtons, betWindowImage]
  | otherwise = pictures [tableWithDealerChip, potWithBoard, playersHands, playersBets] 
  where
    chipImages          = (chipLayout $ images screen)
    playerOnSeatBold p  = pictures [drawPlayerSeatBold p (case active p of
      True  -> seatBoldActive $ images screen
      False -> seatBold       $ images screen),
      drawPlayerName p, drawPlayerBalance p]
    tableWithDealerChip = pictures [background $ images screen, table $ images screen,
      drawDealerChip (dealer screen) chipImages]
    potWithBoard        = pictures [drawPot (calculatePot $ players screen) chipImages,
      drawCardsOnTable (board screen) (front . deckLayout $ images screen)]
    playersHands    = pictures (map (\p -> case action $ move p of
      Bankrupted -> blank
      Folded     -> blank
      _          -> pictures [drawPlayerHand p (deckLayout $ images screen)]) (players screen))
    playersBets     = pictures (map (\p -> pictures [playerOnSeatBold p, drawPlayerBet p chipImages]) (players screen))
    activePlayer    = getActivePlayer $ players screen
    maxBet          = countMaxBet $ players screen
    possibleActions = getPossibleActions activePlayer maxBet
    smallButtons    = case (calculatePot $ players screen) /= 0 && fst possibleActions /= All_In of
      True  -> drawSmallButtons (smallButton $ images screen) (map
        (\buttonText -> betSizeText buttonText) (smallButtonTexts $ images screen))
      False -> blank
    sliderImage    = case fst possibleActions /= All_In of
      True  -> drawSlider (sliderData screen) (slider $ images screen) (sliderBall $ images screen)
      False -> blank
    betWindowImage = case fst possibleActions /= All_In of
      True  -> drawBetWindow (currentValue $ sliderData screen) (betWindow $ images screen)
      False -> blank

-- | Draw player seatbold.
drawPlayerSeatBold :: Player -> Picture -> Picture
drawPlayerSeatBold player img =
  uncurry translate (getSeatBoldOffset $ seat player) img

-- | Draw cards.
drawHand :: Bool -> Maybe (Card, Card) -> DeckLayout -> Picture
drawHand hide hnd layout = case hnd of
  Nothing -> blank
  Just h  -> case hide of
    True  -> pictures [back layout, translate cardOffset 0 (back layout)]
    False -> pictures [front layout !! (fromEnum $ fst h),
      translate cardOffset 0 (front layout !! (fromEnum $ snd h))]

-- | Apply offset for player hand depending on seat.
drawPlayerHand :: Player -> DeckLayout -> Picture
drawPlayerHand player layout = 
  uncurry translate (getHandOffset $ seat player) img
  where
    img = drawHand (hideHand player) (hand player) layout

-- | Draw player name.
drawPlayerName :: Player -> Picture
drawPlayerName player = 
  uncurry translate (getTextNameOffset $ seat player) playerName
  where
    playerName = drawText white (name player)

-- | Draw player balance.
drawPlayerBalance :: Player -> Picture
drawPlayerBalance player = 
  uncurry translate (getBalanceOffset $ seat player) playerBalance
  where
    playerBalance = drawText white
      (show $ balance player - (betSize $ move player))

-- | Draw dealer chip.
drawDealerChip :: Seat -> ChipLayout -> Picture
drawDealerChip s layout = 
  uncurry translate (getDealerChipOffset s) $ dealerChip layout


-- | Draw player bet.
drawPlayerBet :: Player -> ChipLayout -> Picture
drawPlayerBet player layout =
  let bet        = betSize $ move player
      separation = separateBet bet allChipValues
      columns    = length (filter (> 0) separation)
  in pictures [uncurry translate (getChipsOffset (seat player) columns)
    (drawBet 0 separation (stack layout)),
    drawBetSize bet (getChipsOffset (seat player) (-columns))]

-- | Draw column of chips.
drawChipColumn :: Float -> Float -> Int -> Picture -> Picture
drawChipColumn _ _ 0 _                   = blank
drawChipColumn xOffset yOffset height chip = pictures
  [img, drawChipColumn xOffset (yOffset + chipTowerOffset) (height - 1) chip]
  where
    img = translate xOffset yOffset chip

-- | Draw bet in chips by separation and horizontal offset.
drawBet :: Float -> [Int] -> [Chip] -> Picture    
drawBet _ [] [] = blank
drawBet offset separation chips
  | head separation == 0 =
    drawBet offset (tail separation) (tail chips)
  | otherwise =
    pictures [img, drawBet (offset + chipColumnOffset)
              (tail separation) (tail chips)]
  where
    img = drawChipColumn offset 0 (head separation) (sprite $ head chips)

-- | Draw size of bet.
drawBetSize :: Int -> (Float, Float) -> Picture
drawBetSize bet offset
  | bet == 0  = blank
  | otherwise = uncurry translate offset bt
  where
    bt = drawText white (show bet)

-- | Draw pot by it'size.
drawPot :: Int -> ChipLayout -> Picture
drawPot potSize layout
  | potSize == 0 = blank
  | otherwise =
      let separation = separateBet potSize allChipValues
          columns    = length (filter (> 0) separation)
      in pictures [uncurry translate (getPotOffset columns)
        (drawBet 0 separation (stack layout)),
        drawBetSize potSize (getPotOffset (-columns))]

-- | Draw small text. Size is unknown.
drawText :: Color -> String -> Picture
drawText clr string =  color clr $ scale 0.125 0.125 (text string)

-- | Draw cards on table.
drawCardsOnTable :: [Card] -> [Picture] -> Picture
drawCardsOnTable [] _ = blank
drawCardsOnTable cards layout =
  uncurry translate cardsOnTableOffset $ 
    drawRow cardsOnTableRowOffset cardPictures
  where
    cardPictures = map (\card -> layout !! (fromEnum card)) cards

-- | Draw row of pictures.
drawRow :: Float -> [Picture] -> Picture
drawRow _ [] = blank
drawRow offset imgs = pictures [head imgs,
  translate offset 0 (drawRow offset $ tail imgs)]

-- | Draw buttons with possible actions for player.
drawButtons :: (ActionType, ActionType) -> (Picture, Picture) -> [ButtonText] -> Int -> Picture
drawButtons actions buttons texts buttonPressed = 
  translate (-buttonOffset) buttonPositionOffset $ drawRow buttonOffset imgsButtons
  where
    getButtonImg pressedNum = if (buttonPressed /= pressedNum)
      then fst buttons
      else snd buttons
    fstImg = case fst actions of
      Check  -> blank
      _      -> pictures [getButtonImg 1, getActionText texts Fold]
    sndImg   =  pictures [getButtonImg 2, getActionText texts $ fst actions]
    thdImg   = case fst actions of
      All_In -> blank
      _      -> pictures [getButtonImg 3, getActionText texts $ snd actions]
    imgsButtons = [fstImg, sndImg, thdImg]

-- | Draw small buttons.
drawSmallButtons :: Picture -> [Picture] -> Picture
drawSmallButtons img buttons =
  uncurry translate smallButtonPositionOffset $ drawRow smallButtonOffset buttonsWithText
  where
    buttonsWithText = map (\textImage -> pictures [img, textImage]) buttons

-- | Draw slider with possible raise values for player.
drawSlider :: Slider -> Picture -> Picture -> Picture
drawSlider sliderr img ball = 
  uncurry translate sliderOffset $ pictures [img, ballOnSlider]
  where
    ballOnSlider = translate ballOffset 0 ball
    ballOffset   = (-(fst sliderDimensions) + 2 * fst sliderPadding) / 2 +
      (ballPosition sliderr)

-- | Draw bet window with selected bet/raise size.
drawBetWindow :: Int -> Picture -> Picture
drawBetWindow bet img = 
  uncurry translate betWindowOffset (pictures [img, 
    uncurry translate betWindowTextOffset $ (drawText white $ show bet)])

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Calculate margins for window depending on display resolutions.
getMarginsFrom :: (Int, Int) -> (Int, Int)
getMarginsFrom (w, h) = ((w - fst windowSize) `div` 2,
                         (h - snd windowSize) `div` 2)

-- | Separate bet into chip count array.
separateBet :: Int -> [Int] -> [Int]
separateBet _ [] = []
separateBet bet separation
  | partition /= 0 =
        (partition : separateBet remainder (tail separation))
  | otherwise = (0 : separateBet bet       (tail separation))
  where
    partition = bet `div` head separation
    remainder = bet `mod` head separation

-- | Return button text for action.
--   Unsafe function for [] and lists without that action.
getActionText :: [ButtonText] -> ActionType -> Picture
getActionText buttons actionName
  | (actionType $ head buttons) == actionName = actionText $ head buttons
  | otherwise = getActionText (tail buttons) actionName 

-------------------------------------------------------------------------------
-- * Constants
-------------------------------------------------------------------------------

-- | Resolution.
windowSize :: (Int, Int)
windowSize = (960, 720)
