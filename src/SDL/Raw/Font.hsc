module SDL.Raw.Font where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Ptr (Ptr)
import SDL.Raw.Types (Version)

foreign import ccall "SDL_ttf.h TTF_Linked_Version"
  getVersion' :: IO (Ptr Version)

{-# INLINE getVersion #-}
getVersion :: MonadIO m => m (Ptr Version)
getVersion = liftIO getVersion'
