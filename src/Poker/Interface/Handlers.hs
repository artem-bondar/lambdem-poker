-- | Module that operates with input events.
module Poker.Interface.Handlers where

import Poker.Interface.Types

import Poker.Logic.Types   

-- | Operate with user input.
handleInput :: Event -> TableScreen -> TableScreen
handleInput event screen 
  | state screen == Waiting_User_Input = 
    case event of
      EventKey (MouseButton LeftButton) Down _ mouse -> trace (show mouse) screen
      _ -> screen
  | otherwise = screen