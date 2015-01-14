module Game.Toliman.Graphical.SDL.Core(
  sdlCheckRet, sdlCheckRet',
  sdlCheckPtr, sdlCheckPtr',
  sdlCheckPred, sdlCheckPred',
  sdlGetError ) where

import Foreign.C.Types (CInt)
import Foreign.C.String (peekCAString)
import Foreign.Ptr (Ptr, nullPtr)
import Text.Printf (printf)

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Graphics.UI.SDL (getError)
import Monad.Error (throwError)

import Game.Toliman.Graphical.Internal.Errors (
  MonadGraphicalError, TolimanGraphicalError(..))

sdlCheckRet :: (MonadGraphicalError m, MonadIO m) => String -> CInt -> m CInt
sdlCheckRet desc ret
  | ret == -1 = sdlGetError desc
  | otherwise = return ret

sdlCheckRet' :: (MonadGraphicalError m, MonadIO m) => String -> IO CInt -> m CInt
sdlCheckRet' desc m = sdlCheckRet desc =<< (liftIO $ m)

sdlCheckPtr :: (MonadGraphicalError m, MonadIO m) => String -> Ptr a -> m (Ptr a)
sdlCheckPtr desc p
  | p == nullPtr = sdlGetError desc
  | otherwise = return p

sdlCheckPtr' :: (MonadGraphicalError m, MonadIO m) => String -> IO (Ptr a) -> m (Ptr a)
sdlCheckPtr' desc m = sdlCheckPtr desc =<< (liftIO $ m)

sdlCheckPred :: (MonadGraphicalError m, MonadIO m) => String -> Bool -> m ()
sdlCheckPred desc p
  | p = sdlGetError desc
  | otherwise = return ()

sdlCheckPred' :: (MonadGraphicalError m, MonadIO m) => String -> m Bool -> m ()
sdlCheckPred' desc m = sdlCheckPred desc =<< m

sdlGetError :: (MonadGraphicalError m, MonadIO m) => String -> m a
sdlGetError desc = do
    message <- liftIO (getError >>= peekCAString)
    throwError $ SDLError $ printf "%s: %s" desc message
