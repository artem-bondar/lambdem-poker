-- | Responsible for drawing all content on the screen.
module Poker.Interface.Renderer where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Poker.Interface.Types
import Poker.Logic.Types
import Debug.Trace
-------------------------------------------------------------------------------
-- * Render functions
-------------------------------------------------------------------------------

-- | Draw tablescreen.
drawTableScreen :: TableScreen -> Picture
drawTableScreen screen 
    | state screen == Dealing_Hand = pictures
        [background $ images screen,  table $ images screen]
    | otherwise = pictures ([background $ images screen,  table $ images screen,
        drawDealerChip (dealer screen) (chipLayout $ images screen)] ++
        map (\p -> pictures [drawPlayerHand p (deckLayout $ images screen),
                             drawPlayerSeatBold p (seatBold $ images screen),
                             drawPlayerName p, drawPlayerBalance p,
                             drawPlayerBet p (chipLayout $ images screen)])
            (players screen))

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

-- | Draw player name.
drawPlayerName :: Player -> Picture
drawPlayerName player = 
    uncurry translate (getTextNameOffset $ seat player) playerName
    where
        playerName = color white $ scale 0.125 0.125 (text $ name player)

-- | Draw player balance.
drawPlayerBalance :: Player -> Picture
drawPlayerBalance player = 
    uncurry translate (getBalanceOffset $ seat player) playerBalance
    where
        playerBalance = color white $ scale 0.125 0.125 (text . show $ balance player)

-- | Draw dealer chip.
drawDealerChip :: Seat -> ChipLayout -> Picture
drawDealerChip s layout = 
    uncurry translate (getDealerChipOffset s) (dealerChip layout)


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
drawChipColumn xOffset yOffset size chip = pictures
    [img, drawChipColumn xOffset (yOffset + chipTowerOffset) (size - 1) chip]
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
drawBetSize bet offset = uncurry translate offset betSize
    where
        betSize = color white $ scale 0.125 0.125 (text $ show bet)

-------------------------------------------------------------------------------
-- * Utility functions
-------------------------------------------------------------------------------

-- | Calculate margins for window depending on display resolutions.
getMarginsFrom :: (Int, Int) -> (Int, Int)
getMarginsFrom (w, h) = ((w - fst windowSize) `div` 2, (h - snd windowSize) `div` 2)

-- | Separate bet into chip count array.
separateBet :: Int -> [Int] -> [Int]
separateBet _ [] = []
separateBet bet separation
    | partition /= 0 =
        (partition : separateBet remainder (tail separation))
    | otherwise = (0 : separateBet bet (tail separation))
    where
        partition = bet `div` head separation
        remainder = bet `mod` head separation

-- | Return offset for cards depending on seat.
getHandOffset :: Seat -> (Float, Float)
getHandOffset s = case s of
    Bottom     -> (-32, -77)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (-32, 251)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0)

-- | Return offset for seatbold depending on seat.
getSeatBoldOffset :: Seat -> (Float, Float)
getSeatBoldOffset s = case s of
    Bottom     -> (0, -110)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (0, 218)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0) 

 -- | Return offset for seatbold depending on seat.
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
    Bottom     -> (-32, -105)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (-32, 223)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0) 

-- | Return offset for player balance depending on seat.
getBalanceOffset :: Seat -> (Float, Float)
getBalanceOffset s = case s of
    Bottom     -> (-16, -128)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (-16, 200)
    Right_Up   -> (0, 0)
    Right_Down -> (0, 0)

-- | Return chip offset for player bet depending on seat
--   and amount of chip columns.
getChipsOffset :: Seat -> Int -> (Float, Float)
getChipsOffset s columns = case s of
    Bottom     -> (fromIntegral (columns - 1) * (-chipColumnOffset / 2), -12)
    Left_Down  -> (0, 0)
    Left_Up    -> (0, 0)
    Top        -> (fromIntegral (columns - 1) * (-chipColumnOffset / 2), 162)
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

-- | Horizontal offset between chip columns.
chipColumnOffset :: Float
chipColumnOffset = 38

-- | Vertical offset between chips in one column.
chipTowerOffset :: Float
chipTowerOffset = 5
