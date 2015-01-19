
module Game.Toliman.Graphical.UI.Widgets where

import qualified Data.Map.Strict as Map
import qualified Data.RTree.Strict as RTree
import Monad.Ref (getRef, putRef)

import Game.Toliman.Internal.Sodium
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

createWidget :: forall a. ScreenCoord -> UISize -> UIz -> a -> MonadUI UIWidgetID
createWidget _widget_pos _widget_size _widget_z impl = do
  ui' <- getRef
  _widget_behaviour <- sync $ newBehaviour impl
  let
    _widget_id = ui' ^. next_id
    new_widget = UIWidget {..}
    update_map = widgets %~ (Map.insert _widget_id $ MkWidget new_widget)
    update_tree = widget_tree %~ (RTree.insert (widgetMBB new_widget) _widget_id)
    update_id = case _widget_id of UIWidgetID x -> next_id .~ UIWidgetID (x + 1)
  putRef (ui' & (update_map.update_tree.update_id))
  return _widget_id
  where
    mkMBB :: ScreenCoord -> UISize -> RTree.MBB
    mkMBB (Sxy x y) (UIxy x' y') =
      RTree.mbb (fromIntegral x) (fromIntegral y) (fromIntegral $ x + x') (fromIntegral $ y + y')
    widgetMBB :: UIWidget a -> RTree.MBB
    widgetMBB w = mkMBB (w ^. pos) (w ^. size)
