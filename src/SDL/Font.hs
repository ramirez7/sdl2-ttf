{-# LANGUAGE OverloadedStrings #-}

module SDL.Font where

import Control.Exception      (bracket, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Data              (Data)
import Data.List              (find)
import Data.Text              (pack)
import Data.Typeable          (Typeable)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CInt)
import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import SDL                    (Renderer, Texture, Surface(..))
import SDL.Exception          (SDLException(..), throwIfNull, throwIfNeg_)
import SDL.Raw.Filesystem     (rwFromFile, rwFromConstMem)
import SDL.Raw.Types          (RWops)
import System.IO.Unsafe       (unsafePerformIO)
import Control.Monad (unless)

import qualified SDL.Raw
import qualified SDL.Raw.Font

-- | Gets the major, minor, patch versions of the linked @SDL_ttf@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Font.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initializes the library. This must be called before any other part of the
-- library is used. You may call this multiple times.
initialize :: MonadIO m => m ()
initialize = do
  init'd <- (== 1) `fmap` SDL.Raw.Font.wasInit
  unless init'd $
    throwIfNeg_ "SDL.Font.initialize" "TTF_Init" SDL.Raw.Font.init

-- | Cleans up any resources still in use by the library. If called, you must
-- call 'initialize' again before using any other parts of the library.
quit :: MonadIO m => m ()
quit = SDL.Raw.Font.quit
