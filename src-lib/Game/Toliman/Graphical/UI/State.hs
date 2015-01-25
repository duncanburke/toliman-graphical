{-# LANGUAGE RecursiveDo #-}

module Game.Toliman.Graphical.UI.State where

import Control.Monad ((>=>))
import Data.Functor ((<$>))
import Data.Maybe (isNothing)
import Data.Time (secondsToDiffTime)
import qualified FRP.Sodium as Sodium (sync)
import Control.Monad.Lift.IO (liftIO)
import qualified Data.RTree.Strict as RTree

import Game.Toliman.Internal.Lens
import Game.Toliman.Internal.Sodium
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Types

import Game.Toliman.Graphical.UI.Types
import Game.Toliman.Graphical.UI.Events

initUIState :: MonadGraphical ()
initUIState = do
  check "initUIState: isNothing ui" $ isNothing <$> (access ui)
  s <- liftIO $ do
    rec
      _ui_ev_in <- Sodium.sync $ pushEvent
      _ui_time <- Sodium.sync $ setBehaviour $ secondsToDiffTime 0
      _ui_err_bus <- Sodium.sync $ messageBus
      _ui_internal <- Sodium.sync $ reactivePushBehaviour (updateInternalState s) uiInternalStateDefault
      let s = UIState' {..}
    return s
  sync $ dispatchInput s
  (ui._Just) .*= s
  return ()

finalUIState :: MonadGraphical ()
finalUIState = ui .*= Nothing

updateInternalState :: UIState' -> UIInternalEvent -> UIInternalState -> Reactive UIInternalState
updateInternalState st (WidgetChanged {..}) s =
  case update_state of
   Left err -> pushMessage (st^.err_bus) err >> return s
   Right s' -> return s'
  where update_state :: Either TolimanGraphicalError UIInternalState
        update_state = do
          if | (_uiev_old^.widget_handle.widget_id) /= (_uiev_new^.widget_handle.widget_id) -> Left $ UIError "WidgetChanged: widget_id"
             | otherwise -> Right ()
          s & (remove_old >=> insert_new)
        remove_old = id %~ (removeWidget (_uiev_old^.widget_handle) (_uiev_old^.bounds))
        insert_new = id %~ (insertWidget (_uiev_new^.widget_handle) (_uiev_new^.bounds))

updateInternalState st (NewWidget wgt) s = do
  (widget' :: SomeWidget) <- wgt $ s^.next_id
  let inc_id = \s' -> Right (s' & next_id %~ ((\(WidgetID i) -> WidgetID (i+1))))
      insert_widget = id %~ (insertWidget handle' bounds')
      handle' = widget'^.widget_base.widget_handle
      bounds' = widget'^.widget_base.bounds
      update_state = (s & (inc_id >=> insert_widget))
  case update_state of
   Left err -> pushMessage (st^.err_bus) err >> return s
   Right s' -> do
     (handle'^.channel) WidgetCreated
     return s'

removeWidget :: WidgetHandle -> WidgetBounds -> UIInternalState -> Either TolimanGraphicalError UIInternalState
removeWidget hnd b s =  (\t -> s & rtree %~ t) <$> update
  where mbb = (widgetBoundsMBB b)
        matches = concat $ RTree.lookup mbb  (s^.rtree)
        removed = filter (== hnd) matches
        updated = filter (/= hnd) matches
        update = if | length removed == 0 -> Left $ UIError "removeWidget: widget_id not found"
                    | length removed /= 1 -> Left $ UIError "removeWidget: duplicate widget_id"
                    | length updated == 0 -> Right $ RTree.delete mbb
                    | otherwise -> Right $ RTree.insert mbb updated

insertWidget :: WidgetHandle -> WidgetBounds -> UIInternalState -> Either TolimanGraphicalError UIInternalState
insertWidget hnd b s = Right $ s & rtree %~ RTree.insertWith (++) mbb [hnd]
  where mbb = (widgetBoundsMBB b)

lookupWidgetsPoint :: ScreenPos -> UIInternalState -> [WidgetHandle]
lookupWidgetsPoint p s = concat $ RTree.lookupContainsRange (screenPosMBB p) (s^.rtree)
