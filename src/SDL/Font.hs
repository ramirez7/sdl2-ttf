{-|

Module      : SDL.Font
Description : High-level bindings.
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

High-level bindings to the @SDL_ttf@ library.

-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module SDL.Font
  (
  -- * General
    initialize
  , version
  , quit

  -- * Loading fonts
  , Font(..)
  , load
  , free

  -- * Rendering text
  --
  -- | Use the following actions to render text to a 'Surface'. The differing
  -- methods available are described in more detail in the original @SDL_ttf@
  -- documentation
  -- <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC42 here>.
  , Color
  , solid
  , shaded
  , blended
  ) where

import Control.Exception      (bracket, throwIO)
import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Data              (Data)
import Data.List              (find)
import Data.Text              (Text, pack)
import Data.Text.Foreign      (lengthWord16, unsafeCopyToPtr)
import Data.Typeable          (Typeable)
import Data.Word              (Word8, Word16)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CInt, CUShort)
import Foreign.Marshal.Alloc  (allocaBytes, alloca)
import Foreign.Marshal.Utils  (with)
import Foreign.Ptr            (Ptr, castPtr, nullPtr)
import Foreign.Storable       (peek, poke, pokeByteOff)
import GHC.Generics           (Generic)
import Linear                 (V4(..))
import SDL                    (Renderer, Texture, Surface(..))
import SDL.Exception          (SDLException(..), throwIfNull, throwIfNeg_)
import SDL.Raw.Filesystem     (rwFromFile, rwFromConstMem)
import SDL.Raw.Font           (PointSize)
import SDL.Raw.Types          (RWops)
import System.IO.Unsafe       (unsafePerformIO)

import qualified SDL.Raw
import qualified SDL.Raw.Font

-- | Gets the major, minor, patch versions of the linked @SDL_ttf@ library.
-- You may call this without initializing the library with 'initialize'.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Font.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initializes the library. Unless noted otherwise, this must be called
-- before any other part of the library is used. You may call this multiple
-- times.
initialize :: MonadIO m => m ()
initialize = do
  init'd <- (== 1) `fmap` SDL.Raw.Font.wasInit
  unless init'd $
    throwIfNeg_ "SDL.Font.initialize" "TTF_Init" SDL.Raw.Font.init

-- | Cleans up any resources still in use by the library. If called, you must
-- call 'initialize' again before using any other parts of the library.
quit :: MonadIO m => m ()
quit = SDL.Raw.Font.quit

-- | Represents a loaded font.
newtype Font = Font (Ptr SDL.Raw.Font.Font)

-- | Given a path to a @TTF@ or @FON@ file, loads it for use as a 'Font' at a
-- certain 'PointSize'.
load :: MonadIO m => FilePath -> PointSize -> m Font
load path size = do
  fmap Font .
    throwIfNull "SDL.Font.load" "TTF_OpenFont" .
      liftIO . withCString path $ flip SDL.Raw.Font.openFont size

-- | Frees a loaded 'Font'.
free :: MonadIO m => Font -> m ()
free (Font font) = SDL.Raw.Font.closeFont font

-- | Color as an RGBA byte vector.
type Color = V4 Word8

-- | Renders 'Text' using the /quick and dirty/ method. Is the fastest of the
-- rendering methods, but results in text that isn't as /smooth/.
solid :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
solid (Font font) (V4 r g b a) text =
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Solid" .
      liftIO . withText text $ \ptr ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderUNICODE_Solid font (castPtr ptr) fg

-- | Uses the /slow and nice, but with a solid box/ method. Renders slower than
-- 'solid', but in about the same time as 'blended'. Results in a 'Surface'
-- containing antialiased text of a foreground color surrounded by a box of a
-- background color. This 'Surface' will blit as fast as the one from 'solid'.
shaded :: MonadIO m => Font -> Color -> Color -> Text -> m SDL.Surface
shaded (Font font) (V4 r g b a) (V4 r2 g2 b2 a2) text =
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Solid" .
      liftIO . withText text $ \ptr -> do
        with (SDL.Raw.Color r g b a) $ \fg ->
          with (SDL.Raw.Color r2 g2 b2 a2) $ \bg ->
            SDL.Raw.Font.renderUNICODE_Shaded font (castPtr ptr) fg bg

-- | The /slow slow slow, but ultra nice over another image/ method, 'blended'
-- renders text at high quality. The text is antialiased and surrounded by a
-- transparent box. Renders slower than 'solid', but in about the same time as
-- 'shaded'. The resulting 'Surface' will blit slower than the ones from
-- 'solid' or 'shaded'.
blended :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
blended (Font font) (V4 r g b a) text =
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Blended" .
      liftIO . withText text $ \ptr -> do
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderUNICODE_Blended font (castPtr ptr) fg

-- Analogous to Data.Text.Foreign.useAsPtr, just appends a null-byte.
-- FIXME: Is this even necessary?
withText :: Text -> (Ptr Word16 -> IO a) -> IO a
withText text act =
  allocaBytes len $ \ptr -> do
    unsafeCopyToPtr text ptr
    pokeByteOff ptr (len - 2) (0 :: CUShort)
    act ptr
  where
    len = 2*(lengthWord16 text + 1)
