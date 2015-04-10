module SDL.Raw.Font where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..))
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
