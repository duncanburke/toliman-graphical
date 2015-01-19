
module Game.Toliman.Graphical.UI.State
       (runUI) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Control.Monad.Lift.IO (liftIO)

import Monad.Ref (RefT, runRefT)
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.UI.Types
import Game.Toliman.Graphical.Internal.Errors


runUI :: ExceptT TolimanGraphicalError (RefT UIState' IO) a -> MonadGraphical a
runUI m = do
  (s :: UIState') <- getJust "runUI: ui" . accessPrism $ ui._Just
  (e,s') <- liftIO $ flip runRefT s $ runExceptT m
  (ui._Just) .*= s'
  checkError e
