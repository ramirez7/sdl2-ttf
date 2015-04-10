module Main where

import qualified SDL
import qualified SDL.Font

main :: IO ()
main = do
  SDL.Font.initialize
  print =<< SDL.Font.version
  SDL.Font.quit
