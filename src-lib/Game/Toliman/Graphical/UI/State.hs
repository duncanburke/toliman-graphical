
module Game.Toliman.Graphical.UI.State (
  runUI,
  initUIState, finalUIState,
  syncUIState) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Lift.IO (liftIO)
import Data.Functor ((<$>))
import Data.Maybe (isNothing)
import Monad.Ref (RefT, runRefT)

import qualified Data.Map.Strict as Map
import qualified Data.RTree.Strict as RTree

import Game.Toliman.Internal.Lens
import Game.Toliman.Internal.Sodium
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Types

import Game.Toliman.Graphical.UI.Types
import Game.Toliman.Graphical.UI.Events


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
  _ui_ev_in <- sync newEvent
  _ui_bv_state <- sync $ newBehaviour undefined
  _ui_bv_time <- sync $ newBehaviour undefined
  (ui._Just) .*= UIState' {..}
  sync $ widgetEvents (_ui_bv_time ^. behaviour) (_ui_bv_state ^. behaviour) (_ui_ev_in ^. event)

finalUIState :: MonadGraphical ()
finalUIState = ui .*= Nothing

syncUIState :: MonadGraphical ()
syncUIState = do
  s <- getJust "syncUIState: ui" $ access ui
  t <- access time
  sync $ do
    (s ^. bv_time.push) t
    (s ^. bv_state.push) s
  return ()
