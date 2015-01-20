{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.UI.Types where

import Graphics.UI.SDL as SDL
import Data.Map.Strict (Map)
import Data.RTree.Strict (RTree)

import Monad.Ref (MonadRef(..))
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Internal.Sodium

-- | Coordinates in screen-space. `Sxy 0 0` is the
--   bottom-left of the screen. The coordinate of a screen
--   element is measured at the bottom-left corner.

data ScreenPos = Sxy !Int !Int deriving (Show, Eq)

data WidgetPos = Wxy !Int !Int deriving (Show, Eq)

data WidgetSize = WidgetSize !Int !Int deriving (Show, Eq)

-- | Z-level of a widget. Screen-space is a right-handed coordinate system,
-- increasing z moves out of the screen.

newtype WidgetZ = Z Int deriving (Show, Eq, Ord)


data KeyBoardEvent =
  KeyDown Keysym |
  KeyUp Keysym |
  KeyPress Keysym deriving (Show)

data KeyboardState =
  KeyboardState {}
  deriving (Show)

makeUnderscoreFields ''KeyboardState

data MouseButton =
  LeftButton |
  RightButton deriving (Show)

data MouseEvent =
  MouseEnter |
  MouseLeave |
  MouseMove WidgetPos |
  MouseDown WidgetPos MouseButton |
  MouseUp WidgetPos MouseButton |
  MouseClick MouseButton deriving (Show)

data InputEvent =
  SDLEvent SDL.Event
  deriving (Show)

data UIMouseState =
  UIMouseState {}
  deriving (Show)

makeUnderscoreFields ''UIMouseState

newtype WidgetID = WidgetID Int
                   deriving (Show, Eq, Ord);

data Widget a = Widget {
  _widget_id :: !WidgetID,
  _widget_pos :: !ScreenPos,
  _widget_size :: !WidgetSize,
  _widget_z :: !WidgetZ,
  _widget_parent :: !(Maybe WidgetID),
  _widget_behaviour :: !(SodiumBehaviour a)}

makeUnderscoreFields ''Widget

data SomeWidget = forall a. MkWidget (Widget a)

data InputState' =
  Wait |
  MouseLeft

data InputState = InputState {
  _ui_focus :: !(Maybe WidgetID)}

makeUnderscoreFields ''InputState

inputStateDefault :: InputState
inputStateDefault = InputState {
  _ui_focus = Nothing}

data UIState' =
  UIState' {
    _ui_widgets :: !(Map WidgetID SomeWidget),
    _ui_widget_tree :: !(RTree WidgetID),
    _ui_next_id :: !WidgetID,
    _ui_input_channel :: !(SodiumEvent InputEvent),
    _ui_input_state :: !(SodiumBehaviour InputState)}

makeUnderscoreFields ''UIState'

type UIState = Maybe UIState'

uiStateDefault :: UIState
uiStateDefault = Nothing

type MonadUI a =
  forall m.
  (MonadGraphicalError m,
   MonadRef UIState' m) => m a
