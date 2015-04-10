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

module SDL.Font where

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
import Data.Word              (Word8)
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

-- | Represents a loaded font.
newtype Font = Font { fontPtr :: Ptr SDL.Raw.Font.Font }

-- | Loads a file for use as a 'Font', at a certain 'PointSize'. Works with
-- @TTF@ and @FON@ files.
load :: MonadIO m => FilePath -> PointSize -> m Font
load path size = do
  fmap Font .
    throwIfNull "SDL.Font.load" "TTF_OpenFont" .
      liftIO . withCString path $ flip SDL.Raw.Font.openFont size

-- | Frees a loaded 'Font'.
freeFont :: MonadIO m => Font -> m ()
freeFont = SDL.Raw.Font.closeFont . fontPtr

type Color = V4 Word8

-- FIXME: The following functions make sure to append a terminating \0 to all
-- copies of a Text they make. Is this necessary?

renderSolid :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
renderSolid (Font font) (V4 r g b a) text =
  let bytelen = 2*(lengthWord16 text + 1) in
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Solid" .
      liftIO . allocaBytes bytelen $ \textPtr -> do
        unsafeCopyToPtr text textPtr
        pokeByteOff textPtr (bytelen - 2) (0 :: CUShort)
        with (SDL.Raw.Color r g b a) $ \colorPtr ->
          SDL.Raw.Font.renderUNICODE_Solid font (castPtr textPtr) colorPtr

renderShaded :: MonadIO m => Font -> Color -> Color -> Text -> m SDL.Surface
renderShaded (Font font) (V4 r g b a) (V4 r2 g2 b2 a2) text =
  let bytelen = 2*(lengthWord16 text + 1) in
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Solid" .
      liftIO . allocaBytes bytelen $ \textPtr -> do
        unsafeCopyToPtr text textPtr
        pokeByteOff textPtr (bytelen - 2) (0 :: CUShort)
        with (SDL.Raw.Color r g b a) $ \fgPtr ->
          with (SDL.Raw.Color r2 g2 b2 a2) $ \bgPtr ->
            SDL.Raw.Font.renderUNICODE_Shaded font (castPtr textPtr) fgPtr bgPtr

renderBlended :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
renderBlended (Font font) (V4 r g b a) text =
  let bytelen = 2*(lengthWord16 text + 1) in
  fmap SDL.Surface .
    throwIfNull "SDL.Font.render" "TTF_RenderUNICODE_Blended" .
      liftIO . allocaBytes bytelen $ \textPtr -> do
        unsafeCopyToPtr text textPtr
        pokeByteOff textPtr (bytelen - 2) (0 :: CUShort)
        with (SDL.Raw.Color r g b a) $ \colorPtr ->
          SDL.Raw.Font.renderUNICODE_Blended font (castPtr textPtr) colorPtr
