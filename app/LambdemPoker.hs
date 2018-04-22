module Main where

import Interface.TableScreen

main :: IO ()
main = do
    images <- loadImages
    launchTableScreen images
