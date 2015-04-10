module SDL.Font where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Storable       (peek)

import qualified SDL.Raw
import qualified SDL.Raw.Font

-- | Gets the major, minor, patch versions of the linked @SDL_ttf@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Font.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)
