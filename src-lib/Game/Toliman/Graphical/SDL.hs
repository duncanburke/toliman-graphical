module Game.Toliman.Graphical.SDL where

import Foreign.C.Types (CInt)
import Foreign.C.String (peekCAString)
import Text.Printf (printf)

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Graphics.UI.SDL (getError)
import Monad.Error (throwError)

import Game.Toliman.Graphical.Internal.Errors

sdlCheckRet :: (MonadGraphicalError m, MonadIO m) => String -> CInt -> m CInt
sdlCheckRet desc ret = case ret of
  -1 -> sdlGetError desc >>= undefined
  _ -> return ret

sdlGetError :: (MonadGraphicalError m, MonadIO m) => String -> m ()
sdlGetError desc = do
    message <- liftIO (getError >>= peekCAString)
    throwError $ SDLError $ printf "%s: %s" desc message
