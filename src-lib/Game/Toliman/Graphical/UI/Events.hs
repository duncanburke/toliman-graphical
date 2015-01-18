
module Game.Toliman.Graphical.UI.Events where

import Control.Monad.Lift.IO (liftIO)
import FRP.Sodium as Sodium

import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.UI.Types


processEvents :: [UIInputEvent] -> Reactive a -> MonadGraphical ()
processEvents ev r = do
  res <- liftIO $ sync $ do
    r
  return ()
