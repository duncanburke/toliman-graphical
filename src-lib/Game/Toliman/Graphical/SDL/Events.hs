module Game.Toliman.Graphical.SDL.Events where

import Foreign.C.Types (CInt)
import Foreign.C.String (peekCAString)
import Foreign.Ptr (Ptr, nullPtr)
import Text.Printf (printf)

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Graphics.UI.SDL (getError)
import Monad.Error (throwError)

import Game.Toliman.Graphical.Internal.Errors
