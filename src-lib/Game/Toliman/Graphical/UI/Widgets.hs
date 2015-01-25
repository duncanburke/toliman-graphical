
module Game.Toliman.Graphical.UI.Widgets where

import Data.Functor ((<$>))
import qualified Data.Map.Strict as Map
import qualified Data.RTree.Strict as RTree

import Game.Toliman.Internal.Sodium
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

createWidget :: (Widget a) => UIState' -> (WidgetID -> Reactive a) -> Reactive ()
createWidget s wf = (s^.internal.push) $ NewWidget $ (MkWidget <$>) . wf
