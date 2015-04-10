module SDL.Raw.Font where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CLong(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops)

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

type Path = CString

type PointSize = CInt

data Font -- TODO: instance Storable

foreign import ccall "SDL_ttf.h TTF_OpenFont"
  openFont' :: Path -> PointSize -> IO (Ptr Font)

{-# INLINE openFont #-}
openFont :: MonadIO m => Path -> PointSize -> m (Ptr Font)
openFont path = liftIO . openFont' path

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

foreign import ccall "SDL_ttf.h TTF_OpenFontRW"
  openFont_RW' :: Ptr RWops -> Free -> PointSize -> IO (Ptr Font)

{-# INLINE openFont_RW #-}
openFont_RW :: MonadIO m => Ptr RWops -> Free -> PointSize -> m (Ptr Font)
openFont_RW src free = liftIO . openFont_RW' src free

type Index = CLong

foreign import ccall "SDL_ttf.h TTF_OpenFontIndex"
  openFontIndex' :: Path -> PointSize -> Index -> IO (Ptr Font)

{-# INLINE openFontIndex #-}
openFontIndex :: MonadIO m => Path -> PointSize -> Index -> m (Ptr Font)
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
