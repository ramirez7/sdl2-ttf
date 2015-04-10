{-|

Module      : SDL.Font
Description : High-level bindings.
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

High-level bindings to the @SDL_ttf@ library.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module SDL.Font
  (
  -- * General
    initialize
  , version
  , quit

  -- * Loading fonts
  --
  -- | Use the following actions to load @TTF@ and @FON@ file formats.
  , Font(..)
  , PointSize
  , load
  , Index
  , loadIndex
  , decode
  , decodeIndex
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

  -- * Font attributes
  , Style(..)
  , getStyle
  , setStyle
  ) where

import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.&.), (.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Data              (Data)
import Data.Text              (Text)
import Data.Text.Foreign      (lengthWord16, unsafeCopyToPtr)
import Data.Typeable          (Typeable)
import Data.Word              (Word8, Word16)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CUShort, CInt)
import Foreign.Marshal.Alloc  (allocaBytes)
import Foreign.Marshal.Utils  (with)
import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (peek, pokeByteOff)
import GHC.Generics           (Generic)
import Linear                 (V4(..))
import SDL                    (Surface(..))
import SDL.Exception          (throwIfNull, throwIfNeg_)
import SDL.Raw.Filesystem     (rwFromConstMem)
-- import SDL.Raw.Types          (RWops)
-- import System.IO.Unsafe       (unsafePerformIO)

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
  deriving (Eq, Typeable)

-- | Point size (based on 72DPI) to load font as. Translates to pixel height.
type PointSize = Int

-- | Given a path to a font file, loads it for use as a 'Font' at a certain
-- 'PointSize'.
load :: MonadIO m => FilePath -> PointSize -> m Font
load path size = do
  fmap Font .
    throwIfNull "SDL.Font.load" "TTF_OpenFont" .
      liftIO . withCString path $
        flip SDL.Raw.Font.openFont $ fromIntegral size

-- | Same as 'load', but accepts a 'ByteString' containing a font instead.
decode :: MonadIO m => ByteString -> PointSize -> m Font
decode bytes size = liftIO $ do
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap Font .
      throwIfNull "SDL.Font.decode" "TTF_OpenFontRW" $
        SDL.Raw.Font.openFont_RW rw 0 $ fromIntegral size

-- | Designates a font face, the default and first one being 0.
type Index = Int

-- | Given a path to a font file, loads one of its font faces (designated by
-- the given index) for use as a 'Font' at a certain 'PointSize'. The first
-- face is always index 0, and is the one chosen by default when using 'load'.
loadIndex :: MonadIO m => FilePath -> PointSize -> Index -> m Font
loadIndex path size i = do
  fmap Font .
    throwIfNull "SDL.Font.loadIndex" "TTF_OpenFontIndex" .
      liftIO . withCString path $ \cpath ->
        SDL.Raw.Font.openFontIndex cpath (fromIntegral size) (fromIntegral i)

-- | Same as 'loadIndex', but accepts a 'ByteString' containing a font instead.
decodeIndex :: MonadIO m => ByteString -> PointSize -> Index -> m Font
decodeIndex bytes size i = liftIO $ do
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap Font .
      throwIfNull "SDL.Font.decodeIndex" "TTF_OpenFontIndexRW" $
        SDL.Raw.Font.openFontIndex_RW rw 0 (fromIntegral size) (fromIntegral i)

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

-- Helper function for converting a bitmask into a list of values.
fromMaskWith :: (Enum a, Bounded a) => (a -> CInt) -> CInt -> [a]
fromMaskWith convert cint = concatMap (\a -> find (a, convert a)) $ [minBound..]
  where
    find (a, i) = if i == i .&. cint then [a] else []

-- Helper function for converting a list of values into a bitmask.
toMaskWith :: (a -> CInt) -> [a] -> CInt
toMaskWith convert = foldr (.|.) 0 . map convert

-- | Possible styles that can be applied to a 'Font'.
data Style
  = Normal
  | Bold
  | Italic
  | Underline
  | Strikethrough
  deriving (Eq, Enum, Ord, Bounded, Data, Generic, Typeable, Read, Show)

styleToCInt :: Style -> CInt
styleToCInt =
  \case
    Normal        -> SDL.Raw.Font.TTF_STYLE_NORMAL
    Bold          -> SDL.Raw.Font.TTF_STYLE_BOLD
    Italic        -> SDL.Raw.Font.TTF_STYLE_ITALIC
    Underline     -> SDL.Raw.Font.TTF_STYLE_UNDERLINE
    Strikethrough -> SDL.Raw.Font.TTF_STYLE_STRIKETHROUGH

-- | Gets the rendering style of a given font. If none was ever set, this
-- will be a list containing only 'Normal'.
getStyle :: MonadIO m => Font -> m [Style]
getStyle (Font font) = do
  cint <- SDL.Raw.Font.getFontStyle font
  return $ fromMaskWith styleToCInt cint

-- | Sets the rendering style of a font. If none is given, the default of
-- 'Normal' will be used.
setStyle :: MonadIO m => Font -> [Style] -> m ()
setStyle (Font font) = SDL.Raw.Font.setFontStyle font . toMaskWith styleToCInt
