module Main where

import qualified SDL
import qualified SDL.Font

main :: IO ()
main = do
  v <- SDL.Font.version
  print v
