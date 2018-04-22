module Main where

import Interface.Renderer
import Interface.TableScreen

main :: IO ()
main = do
    images <- loadImages
    launchTableScreen images
