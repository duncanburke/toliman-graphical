
module Game.Toliman.Graphical.UI.State (
  runUI,
  initUIState, finalUIState) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor ((<$>))
import Data.Maybe (isNothing)

import Control.Monad.Lift.IO (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.RTree.Strict as RTree

import Monad.Ref (RefT, runRefT)
import Game.Toliman.Internal.Lens
import Game.Toliman.Internal.Sodium
import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.UI.Types
import Game.Toliman.Graphical.Internal.Errors

runUI :: ExceptT TolimanGraphicalError (RefT UIState' IO) a -> MonadGraphical a
runUI m = do
  (s :: UIState') <- getJust "runUI: ui" $ accessPrism $ ui._Just
  (e,s') <- liftIO $ flip runRefT s $ runExceptT m
  (ui._Just) .*= s'
  checkError e

initUIState :: MonadGraphical ()
initUIState = do
  check "initUIState: isNothing ui" $ isNothing <$> (access ui)
  let
    _ui_widgets =  Map.empty
    _ui_widget_tree = RTree.empty
    _ui_next_id = WidgetID 0
  _ui_input_channel <- sync newEvent
  _ui_input_state <- sync $ newBehaviour inputStateDefault
  (ui._Just) .*= UIState' {..}

finalUIState :: MonadGraphical ()
finalUIState = ui .*= Nothing

