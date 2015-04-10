{-|

Module      : SDL.Raw.Font
Description : Low-level bindings.
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Low-level bindings to the @SDL_ttf@ library. No error-handling is done here.
For more information about specific function behaviour, see the @SDL_ttf@
documentation.

-}

{-# LANGUAGE PatternSynonyms #-}

module SDL.Raw.Font
  (
  -- * General
    init
  , wasInit
  , quit
  , getVersion

  -- * Loading fonts
  , Font
  , FontPath
  , PointSize
  , openFont
  , Free
  , openFont_RW
  , Index
  , openFontIndex
  , openFontIndex_RW
  , closeFont

  -- * Font attributes
  , getFontStyle
  , setFontStyle
  , pattern TTF_STYLE_NORMAL
  , pattern TTF_STYLE_BOLD
  , pattern TTF_STYLE_ITALIC
  , pattern TTF_STYLE_STRIKETHROUGH
  , getFontOutline
  , setFontOutline
  , getFontHinting
  , setFontHinting
  , pattern TTF_HINTING_NORMAL
  , pattern TTF_HINTING_LIGHT
  , pattern TTF_HINTING_MONO
  , pattern TTF_HINTING_NONE
  , getFontKerning
  , setFontKerning
  , fontHeight
  , fontAscent
  , fontDescent
  , fontLineSkip
  , fontFaces
  , fontFaceIsFixedWidth
  , fontFaceFamilyName
  , fontFaceStyleName
  , glyphIsProvided
  , glyphMetrics

  -- * Getting text size
  , sizeText
  , sizeUTF8
  , sizeUNICODE

  -- * Rendering text
  , renderText_Solid
  , renderText_Shaded
  , renderText_Blended
  , renderUTF8_Solid
  , renderUTF8_Shaded
  , renderUTF8_Blended
  , renderUNICODE_Solid
  , renderUNICODE_Shaded
  , renderUNICODE_Blended
  , renderGlyph_Solid
  , renderGlyph_Shaded
  , renderGlyph_Blended

  -- * Other
  , byteSwappedUNICODE
  , pattern UNICODE_BOM_NATIVE
  , pattern UNICODE_BOM_SWAPPED
  ) where

#include "SDL_ttf.h"

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CLong(..), CUShort(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops, Color)

foreign import ccall "SDL_ttf.h TTF_Linked_Version"
  getVersion' :: IO (Ptr Version)

{-# INLINE getVersion #-}
getVersion :: MonadIO m => m (Ptr Version)
getVersion = liftIO getVersion'

foreign import ccall "SDL_ttf.h TTF_Init"
  init' :: IO CInt

{-# INLINE init #-}
init :: MonadIO m => m CInt
init = liftIO init'

foreign import ccall "SDL_ttf.h TTF_WasInit"
  wasInit' :: IO CInt

{-# INLINE wasInit #-}
wasInit :: MonadIO m => m CInt
wasInit = liftIO wasInit'

foreign import ccall "SDL_ttf.h TTF_Quit"
  quit' :: IO ()

{-# INLINE quit #-}
quit :: MonadIO m => m ()
quit = liftIO quit'

type FontPath = CString

-- | Point size (based on 72DPI). Translates to pixel height.
type PointSize = CInt

-- | The raw, underlying @TTF_Font@ struct.
data Font

foreign import ccall "SDL_ttf.h TTF_OpenFont"
  openFont' :: FontPath -> PointSize -> IO (Ptr Font)

{-# INLINE openFont #-}
openFont :: MonadIO m => FontPath -> PointSize -> m (Ptr Font)
openFont path = liftIO . openFont' path

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

foreign import ccall "SDL_ttf.h TTF_OpenFontRW"
  openFont_RW' :: Ptr RWops -> Free -> PointSize -> IO (Ptr Font)

{-# INLINE openFont_RW #-}
openFont_RW :: MonadIO m => Ptr RWops -> Free -> PointSize -> m (Ptr Font)
openFont_RW src free = liftIO . openFont_RW' src free

-- | Indicates the font face we're loading. First face is always 0.
type Index = CLong

foreign import ccall "SDL_ttf.h TTF_OpenFontIndex"
  openFontIndex' :: FontPath -> PointSize -> Index -> IO (Ptr Font)

{-# INLINE openFontIndex #-}
openFontIndex :: MonadIO m => FontPath -> PointSize -> Index -> m (Ptr Font)
openFontIndex path index = liftIO . openFontIndex' path index

foreign import ccall "SDL_ttf.h TTF_OpenFontIndexRW"
  openFontIndex_RW' :: Ptr RWops -> Free -> PointSize -> Index -> IO (Ptr Font)

{-# INLINE openFontIndex_RW #-}
openFontIndex_RW :: MonadIO m => Ptr RWops -> Free -> PointSize -> Index -> m (Ptr Font)
openFontIndex_RW src free ptsize = liftIO . openFontIndex_RW' src free ptsize

foreign import ccall "SDL_ttf.h TTF_CloseFont"
  closeFont' :: Ptr Font -> IO ()

{-# INLINE closeFont #-}
closeFont :: MonadIO m => Ptr Font -> m ()
closeFont = liftIO . closeFont'

foreign import ccall "SDL_ttf.h TTF_ByteSwappedUNICODE"
  byteSwappedUNICODE' :: CInt -> IO ()

{-# INLINE byteSwappedUNICODE #-}
byteSwappedUNICODE :: MonadIO m => CInt -> m ()
byteSwappedUNICODE = liftIO . byteSwappedUNICODE'

pattern UNICODE_BOM_NATIVE  = #{const UNICODE_BOM_NATIVE}
pattern UNICODE_BOM_SWAPPED = #{const UNICODE_BOM_SWAPPED}

foreign import ccall "SDL_ttf.h TTF_GetFontStyle"
  getFontStyle' :: Ptr Font -> IO CInt

{-# INLINE getFontStyle #-}
getFontStyle :: MonadIO m => Ptr Font -> m CInt
getFontStyle = liftIO . getFontStyle'

foreign import ccall "SDL_ttf.h TTF_SetFontStyle"
  setFontStyle' :: Ptr Font -> CInt -> IO ()

{-# INLINE setFontStyle #-}
setFontStyle :: MonadIO m => Ptr Font -> CInt -> m ()
setFontStyle font = liftIO . setFontStyle' font

pattern TTF_STYLE_BOLD          = #{const TTF_STYLE_BOLD}
pattern TTF_STYLE_ITALIC        = #{const TTF_STYLE_ITALIC}
pattern TTF_STYLE_NORMAL        = #{const TTF_STYLE_NORMAL}
pattern TTF_STYLE_STRIKETHROUGH = #{const TTF_STYLE_STRIKETHROUGH}

foreign import ccall "SDL_ttf.h TTF_GetFontOutline"
  getFontOutline' :: Ptr Font -> IO CInt

{-# INLINE getFontOutline #-}
getFontOutline :: MonadIO m => Ptr Font -> m CInt
getFontOutline = liftIO . getFontOutline'

foreign import ccall "SDL_ttf.h TTF_SetFontOutline"
  setFontOutline' :: Ptr Font -> CInt -> IO ()

{-# INLINE setFontOutline #-}
setFontOutline :: MonadIO m => Ptr Font -> CInt -> m ()
setFontOutline font = liftIO . setFontOutline' font

foreign import ccall "SDL_ttf.h TTF_GetFontHinting"
  getFontHinting' :: Ptr Font -> IO CInt

{-# INLINE getFontHinting #-}
getFontHinting :: MonadIO m => Ptr Font -> m CInt
getFontHinting = liftIO . getFontHinting'

foreign import ccall "SDL_ttf.h TTF_SetFontHinting"
  setFontHinting' :: Ptr Font -> CInt -> IO ()

{-# INLINE setFontHinting #-}
setFontHinting :: MonadIO m => Ptr Font -> CInt -> m ()
setFontHinting font = liftIO . setFontHinting' font

pattern TTF_HINTING_LIGHT  = #{const TTF_HINTING_LIGHT}
pattern TTF_HINTING_MONO   = #{const TTF_HINTING_MONO}
pattern TTF_HINTING_NONE   = #{const TTF_HINTING_NONE}
pattern TTF_HINTING_NORMAL = #{const TTF_HINTING_NORMAL}

foreign import ccall "SDL_ttf.h TTF_GetFontKerning"
  getFontKerning' :: Ptr Font -> IO CInt

{-# INLINE getFontKerning #-}
getFontKerning :: MonadIO m => Ptr Font -> m CInt
getFontKerning = liftIO . getFontKerning'

foreign import ccall "SDL_ttf.h TTF_SetFontKerning"
  setFontKerning' :: Ptr Font -> CInt -> IO ()

{-# INLINE setFontKerning #-}
setFontKerning :: MonadIO m => Ptr Font -> CInt -> m ()
setFontKerning font = liftIO . setFontKerning' font

foreign import ccall "SDL_ttf.h TTF_FontHeight"
  fontHeight' :: Ptr Font -> IO CInt

{-# INLINE fontHeight #-}
fontHeight :: MonadIO m => Ptr Font -> m CInt
fontHeight = liftIO . fontHeight'

foreign import ccall "SDL_ttf.h TTF_FontAscent"
  fontAscent' :: Ptr Font -> IO CInt

{-# INLINE fontAscent #-}
fontAscent :: MonadIO m => Ptr Font -> m CInt
fontAscent = liftIO . fontAscent'

foreign import ccall "SDL_ttf.h TTF_FontDescent"
  fontDescent' :: Ptr Font -> IO CInt

{-# INLINE fontDescent #-}
fontDescent :: MonadIO m => Ptr Font -> m CInt
fontDescent = liftIO . fontDescent'

foreign import ccall "SDL_ttf.h TTF_FontLineSkip"
  fontLineSkip' :: Ptr Font -> IO CInt

{-# INLINE fontLineSkip #-}
fontLineSkip :: MonadIO m => Ptr Font -> m CInt
fontLineSkip = liftIO . fontLineSkip'

foreign import ccall "SDL_ttf.h TTF_FontFaces"
  fontFaces' :: Ptr Font -> IO CLong

{-# INLINE fontFaces #-}
fontFaces :: MonadIO m => Ptr Font -> m CLong
fontFaces = liftIO . fontFaces'

foreign import ccall "SDL_ttf.h TTF_FontFaceIsFixedWidth"
  fontFaceIsFixedWidth' :: Ptr Font -> IO CInt

{-# INLINE fontFaceIsFixedWidth #-}
fontFaceIsFixedWidth :: MonadIO m => Ptr Font -> m CInt
fontFaceIsFixedWidth = liftIO . fontFaceIsFixedWidth'

foreign import ccall "SDL_ttf.h TTF_FontFaceFamilyName"
  fontFaceFamilyName' :: Ptr Font -> IO CString

{-# INLINE fontFaceFamilyName #-}
fontFaceFamilyName :: MonadIO m => Ptr Font -> m CString
fontFaceFamilyName = liftIO . fontFaceFamilyName'

foreign import ccall "SDL_ttf.h TTF_FontFaceStyleName"
  fontFaceStyleName' :: Ptr Font -> IO CString

{-# INLINE fontFaceStyleName #-}
fontFaceStyleName :: MonadIO m => Ptr Font -> m CString
fontFaceStyleName = liftIO . fontFaceStyleName'

foreign import ccall "SDL_ttf.h TTF_GlyphIsProvided"
  glyphIsProvided' :: Ptr Font -> CUShort -> IO CInt

{-# INLINE glyphIsProvided #-}
glyphIsProvided :: MonadIO m => Ptr Font -> CUShort -> m CInt
glyphIsProvided font = liftIO . glyphIsProvided' font

foreign import ccall "SDL_ttf.h TTF_GlyphMetrics"
  glyphMetrics'
    :: Ptr Font -> CUShort ->
       Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt ->
       IO CInt

{-# INLINE glyphMetrics #-}
glyphMetrics
    :: MonadIO m =>
       Ptr Font -> CUShort ->
       Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt ->
       m CInt
glyphMetrics font char minx maxx miny maxy advance =
  liftIO $ glyphMetrics' font char minx maxx miny maxy advance

foreign import ccall "SDL_ttf.h TTF_SizeText"
  sizeText' :: Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

{-# INLINE sizeText #-}
sizeText :: MonadIO m => Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> m CInt
sizeText font text w h = liftIO $ sizeText' font text w h

foreign import ccall "SDL_ttf.h TTF_SizeUTF8"
  sizeUTF8' :: Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt

{-# INLINE sizeUTF8 #-}
sizeUTF8 :: MonadIO m => Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> m CInt
sizeUTF8 font text w h = liftIO $ sizeUTF8' font text w h

foreign import ccall "SDL_ttf.h TTF_SizeUNICODE"
  sizeUNICODE' :: Ptr Font -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> IO CInt

{-# INLINE sizeUNICODE #-}
sizeUNICODE
  :: MonadIO m => Ptr Font -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> m CInt
sizeUNICODE font text w h = liftIO $ sizeUNICODE' font text w h

foreign import ccall "SDL_ttf.h TTF_RenderText_Solid_p"
  renderText_Solid' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderText_Solid #-}
renderText_Solid
  :: MonadIO m => Ptr Font -> CString -> Ptr Color -> m (Ptr Surface)
renderText_Solid font text fg = liftIO $ renderText_Solid' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderUTF8_Solid_p"
  renderUTF8_Solid' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUTF8_Solid #-}
renderUTF8_Solid
  :: MonadIO m => Ptr Font -> CString -> Ptr Color -> m (Ptr Surface)
renderUTF8_Solid font text fg = liftIO $ renderUTF8_Solid' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderUNICODE_Solid_p"
  renderUNICODE_Solid'
    :: Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUNICODE_Solid #-}
renderUNICODE_Solid
  :: MonadIO m => Ptr Font -> Ptr CUShort -> Ptr Color -> m (Ptr Surface)
renderUNICODE_Solid font text fg = liftIO $ renderUNICODE_Solid' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderGlyph_Solid_p"
  renderGlyph_Solid' :: Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderGlyph_Solid #-}
renderGlyph_Solid
  :: MonadIO m => Ptr Font -> CUShort -> Ptr Color -> m (Ptr Surface)
renderGlyph_Solid font text fg = liftIO $ renderGlyph_Solid' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderText_Shaded_p"
  renderText_Shaded'
    :: Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderText_Shaded #-}
renderText_Shaded
  :: MonadIO m =>
     Ptr Font -> CString -> Ptr Color -> Ptr Color -> m (Ptr Surface)
renderText_Shaded font text fg bg = liftIO $ renderText_Shaded' font text fg bg

foreign import ccall "SDL_ttf.h TTF_RenderUTF8_Shaded_p"
  renderUTF8_Shaded'
    :: Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUTF8_Shaded #-}
renderUTF8_Shaded
  :: MonadIO m =>
     Ptr Font -> CString -> Ptr Color -> Ptr Color -> m (Ptr Surface)
renderUTF8_Shaded font text fg bg = liftIO $ renderUTF8_Shaded' font text fg bg

foreign import ccall "SDL_ttf.h TTF_RenderUNICODE_Shaded_p"
  renderUNICODE_Shaded'
    :: Ptr Font -> Ptr CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUNICODE_Shaded #-}
renderUNICODE_Shaded
  :: MonadIO m =>
     Ptr Font -> Ptr CUShort -> Ptr Color -> Ptr Color -> m (Ptr Surface)
renderUNICODE_Shaded font text fg bg =
  liftIO $ renderUNICODE_Shaded' font text fg bg

foreign import ccall "SDL_ttf.h TTF_RenderGlyph_Shaded_p"
  renderGlyph_Shaded'
    :: Ptr Font -> CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderGlyph_Shaded #-}
renderGlyph_Shaded
  :: MonadIO m =>
     Ptr Font -> CUShort -> Ptr Color -> Ptr Color -> m (Ptr Surface)
renderGlyph_Shaded font text fg bg =
  liftIO $ renderGlyph_Shaded' font text fg bg

foreign import ccall "SDL_ttf.h TTF_RenderText_Blended_p"
  renderText_Blended' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderText_Blended #-}
renderText_Blended
  :: MonadIO m => Ptr Font -> CString -> Ptr Color -> m (Ptr Surface)
renderText_Blended font text fg = liftIO $ renderText_Blended' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderUTF8_Blended_p"
  renderUTF8_Blended' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUTF8_Blended #-}
renderUTF8_Blended
  :: MonadIO m => Ptr Font -> CString -> Ptr Color -> m (Ptr Surface)
renderUTF8_Blended font text fg = liftIO $ renderUTF8_Blended' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderUNICODE_Blended_p"
  renderUNICODE_Blended'
    :: Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderUNICODE_Blended #-}
renderUNICODE_Blended
  :: MonadIO m => Ptr Font -> Ptr CUShort -> Ptr Color -> m (Ptr Surface)
renderUNICODE_Blended font text fg =
  liftIO $ renderUNICODE_Blended' font text fg

foreign import ccall "SDL_ttf.h TTF_RenderGlyph_Blended_p"
  renderGlyph_Blended' :: Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)

{-# INLINE renderGlyph_Blended #-}
renderGlyph_Blended
  :: MonadIO m => Ptr Font -> CUShort -> Ptr Color -> m (Ptr Surface)
renderGlyph_Blended font text fg = liftIO $ renderGlyph_Blended' font text fg
