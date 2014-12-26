module Game.Toliman.Graphical.SDL.Core where

import Foreign.C.Types (CInt)
import Foreign.C.String (peekCAString)
import Foreign.Ptr (Ptr, nullPtr)
import Text.Printf (printf)

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Graphics.UI.SDL (getError)
import Monad.Error (throwError)

import Game.Toliman.Graphical.Internal.Errors

sdlCheckRet :: (MonadGraphicalError m, MonadIO m) => String -> CInt -> m CInt
sdlCheckRet desc ret
  | ret == -1 = sdlGetError desc >>= undefined
  | otherwise = return ret

sdlCheckRet' :: (MonadGraphicalError m, MonadIO m) => String -> IO CInt -> m CInt
sdlCheckRet' desc m = sdlCheckRet desc =<< (liftIO $ m)

sdlCheckNull :: (MonadGraphicalError m, MonadIO m) => String -> Ptr a -> m (Ptr a)
sdlCheckNull desc p
  | p == nullPtr = sdlGetError desc >>= undefined
  | otherwise = return p

sdlCheckNull' :: (MonadGraphicalError m, MonadIO m) => String -> IO (Ptr a) -> m (Ptr a)
sdlCheckNull' desc m = sdlCheckNull desc =<< (liftIO $ m)

sdlGetError :: (MonadGraphicalError m, MonadIO m) => String -> m ()
sdlGetError desc = do
    message <- liftIO (getError >>= peekCAString)
    throwError $ SDLError $ printf "%s: %s" desc message
