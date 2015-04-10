{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Text          (Text)
import Data.Text.IO       (putStrLn)
import Linear             (V4(..))
import Prelude     hiding (putStrLn)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Font

red :: SDL.Font.Color
red = V4 maxBound minBound minBound maxBound

-- A sequence of example actions to be perfomed and displayed.
examples :: [(Text, SDL.Window -> FilePath -> IO ())]
examples = [
  ("Loading font, blitting some text",
    \window path -> do
      font   <- SDL.Font.load path 70
      text   <- SDL.Font.blended font red "Why hello there!"
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.blitSurface text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window)]

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  getArgs >>= \case

    [] -> do
      putStrLn "Usage: cabal run path/to/font.(ttf|fon)"
      exitFailure

    -- Run each of the examples within a newly-created window.
    (path:_) -> do
      flip mapM_ examples $ \(name, action) -> do
        putStrLn name
        window <- SDL.createWindow name SDL.defaultWindow
        SDL.showWindow window
        action window path
        threadDelay 1000000
        SDL.destroyWindow window

  SDL.Font.quit
  SDL.quit
