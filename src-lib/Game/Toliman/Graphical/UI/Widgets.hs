
module Game.Toliman.Graphical.UI.Widgets where

import qualified Data.Map.Strict as Map
import qualified Data.RTree.Strict as RTree
import Monad.State (get, put)

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

createWidget :: forall a. (UIWidgetID -> UIWidget a) -> MonadUI UIWidgetID
createWidget f = do
  ui' <- get
  let
    widget_id = ui' ^. next_id
    new_widget = f widget_id
    update_map = widgets %~ (Map.insert widget_id $ MkWidget new_widget)
    update_tree = widget_tree %~ (RTree.insert (widgetMBB new_widget) widget_id)
    update_id = case widget_id of UIWidgetID x -> next_id .~ UIWidgetID (x + 1)
  put (ui' & (update_map.update_tree.update_id))
  return widget_id
  where
    mkMBB :: ScreenCoord -> UISize -> RTree.MBB
    mkMBB (Sxy x y) (UIxy x' y') =
      RTree.mbb (fromIntegral x) (fromIntegral y) (fromIntegral $ x + x') (fromIntegral $ y + y')
    widgetMBB :: UIWidget a -> RTree.MBB
    widgetMBB w = mkMBB (w ^. pos) (w ^. size)
