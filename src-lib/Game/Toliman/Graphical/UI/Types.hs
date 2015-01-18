{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Game.Toliman.Graphical.UI.Types where

import Graphics.UI.SDL as SDL
import Data.Map.Strict (Map)
import Data.RTree.Strict (RTree)
import Monad.State (MonadState)

import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Internal.Sodium

-- | Coordinates in screen-space. `Sxy 0 0` is the
--   bottom-left of the screen. The coordinate of a screen
--   element is measured at the bottom-left corner.

data ScreenCoord = Sxy !Int !Int
                 deriving (Show, Eq)

data UISize = UIxy !Int !Int
            deriving (Show, Eq)

newtype UIz = UIz Int
              deriving (Show, Eq, Ord)


data UIKeyBoardEvent =
  KeyDown Keysym |
  KeyUp Keysym |
  KeyPress Keysym deriving (Show)


data UIKeyboardState =
  UIKeyboardState {}
  deriving (Show)

makeUnderscoreFields ''UIKeyboardState

data UIMouseButton =
  MouseLeft |
  MouseRight deriving (Show)

data UIMouseEvent =
  MouseEnter |
  MouseLeave |
  MouseMove ScreenCoord |
  MouseDown ScreenCoord UIMouseButton |
  MouseUp ScreenCoord UIMouseButton |
  MouseClick UIMouseButton deriving (Show)

data UIInputEvent =
  UIKeyBoardEvent |
  UIMouseEvent deriving (Show)

data UIMouseState =
  UIMouseState {}
  deriving (Show)

makeUnderscoreFields ''UIMouseState

newtype UIWidgetID = UIWidgetID Int
                   deriving (Show, Eq, Ord);

data UIWidget a = UIWidget {
  _widget_id :: !UIWidgetID,
  _widget_pos :: !ScreenCoord,
  _widget_size :: !UISize,
  _widget_z :: !UIz,
  _widget_impl :: a }

makeUnderscoreFields ''UIWidget

data UISomeWidget = forall a. MkWidget (UIWidget a)

data UIState' =
  UIState' {
    _ui_reactor :: !(Reactive ()),
    _ui_widgets :: Map UIWidgetID UISomeWidget,
    _ui_widget_tree :: RTree UIWidgetID,
    _ui_next_id :: UIWidgetID }

makeUnderscoreFields ''UIState'

type MonadUI a = forall m. MonadState UIState' m => m a

type UIState = Maybe UIState'

uiStateDefault :: UIState
uiStateDefault = Nothing
