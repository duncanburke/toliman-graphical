{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Game.Toliman.Graphical.UI.Types where

import Graphics.UI.SDL as SDL
import Data.RTree.Strict as RTree
import Data.Time.Clock (DiffTime)
import Control.Lens

import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Internal.Sodium


-- | Coordinates in screen-space. `Sxy 0 0` is the
-- bottom-left of the screen. The coordinate of a screen
-- element is measured at the bottom-left corner.
data ScreenPos =
  Sxy {
    _sxy_x :: !Int,
    _sxy_y :: !Int} deriving (Show, Eq)

makeUnderscoreFields ''ScreenPos

screenPosMBB :: ScreenPos -> RTree.MBB
screenPosMBB p = mbb x' y' x' y'
  where x' = fromIntegral (p^.x)
        y' = fromIntegral (p^. y)


data WidgetPos =
  Wxy {
    _wxy_x :: !Int,
    _wxy_y :: !Int} deriving (Show, Eq)

makeUnderscoreFields ''WidgetPos


data WidgetBounds =
  WBounds {
    -- | 'x' of bottom-left corner in screen-space
    _wb_x :: !Int,
    -- | 'y' of bottom-left corner in screen-space
    _wb_y :: !Int,
    -- | 'width'
    _wb_w :: !Int,
    -- | 'height'
    _wb_h :: !Int,
    -- | 'z' position in right-handed screen-space (z pointing out of screen)
    _wb_z :: !Int} deriving (Show, Eq)

makeUnderscoreFields ''WidgetBounds

widgetBoundsMBB :: WidgetBounds -> RTree.MBB
widgetBoundsMBB b = mbb x' y' (x'+w') (y'+h')
  where x' = fromIntegral (b^.x)
        y' = fromIntegral (b^.y)
        w' = fromIntegral (b^.w)
        h' = fromIntegral (b^.y)

data InputEvent =
  SDLEvent !SDL.Event
  deriving (Show)

newtype WidgetID = WidgetID Int
                   deriving (Show, Eq, Ord)

data MouseButton = LeftButton | RightButton deriving (Show)

data WidgetEvent =
  WidgetCreated |
  HaveFocus |
  LostFocus |
  MouseMove WidgetPos |
  MouseEnter |
  MouseLeave |
  MouseDown MouseButton |
  MouseCancel |
  MouseUp MouseButton |
  MouseClick MouseButton |
  MouseDoubleClick |
  MouseBox WidgetPos WidgetPos |
  MouseBoxCancel |
  MouseBoxComplete
  deriving (Show)


data WidgetBase =
  WidgetBase {
    _wgt_widget_id :: !WidgetID,
    _wgt_bounds :: !WidgetBounds,
    _wgt_behaviour :: Behaviour SomeWidget,
    _wgt_channel :: WidgetEvent -> Reactive ()}

class Widget a where
  widgetBase :: a -> WidgetBase
  handleEvent :: WidgetEvent -> a -> Reactive a

data SomeWidget = forall a. Widget a => MkWidget a

makeUnderscoreFields ''WidgetBase

class Has_widget_base s a | s -> a where
  widget_base :: Getter s a

instance Has_widget_base SomeWidget WidgetBase where
  widget_base = to (\(MkWidget w) -> widgetBase w)

class Has_widget_mbb s a | s -> a where
  widget_mbb :: Getter s a

instance Has_widget_mbb WidgetBase RTree.MBB where
  widget_mbb = to (\b -> widgetBoundsMBB (b^.bounds))

data InputState' =
  Wait |
  MouseLeft


data InputState = InputState {
  _ui_focus :: !(Maybe WidgetID)}

inputStateDefault :: InputState
inputStateDefault = InputState {
  _ui_focus = Nothing}

makeUnderscoreFields ''InputState


data WidgetHandle = WidgetHandle {
  _whnd_widget_id :: !WidgetID,
  _whnd_behaviour :: !(Behaviour SomeWidget),
  _whnd_channel :: !(WidgetEvent -> Reactive ())}

makeUnderscoreFields ''WidgetHandle

instance Eq WidgetHandle where
  h1 == h2 = h1^.widget_id == h2^.widget_id

class Has_widget_handle s a | s -> a where
  widget_handle :: Getter s a

instance Has_widget_handle WidgetBase WidgetHandle where
  widget_handle = to (\b -> WidgetHandle (b^.widget_id) (b^.behaviour) (b^.channel))

data UIInternalEvent =
  WidgetChanged { _uiev_old :: WidgetBase,
                  _uiev_new :: WidgetBase} |
  NewWidget (WidgetID -> Reactive SomeWidget)

makeUnderscoreFields ''UIInternalEvent


data UIInternalState =
  UIInternalState {
    _ui_rtree :: !(RTree [WidgetHandle]),
    _ui_next_id :: !WidgetID}

uiInternalStateDefault :: UIInternalState
uiInternalStateDefault = UIInternalState {
  _ui_rtree = RTree.empty,
  _ui_next_id = WidgetID 0}

makeUnderscoreFields ''UIInternalState


data UIState'  =
  UIState' {
    _ui_internal :: !(ReactivePushBehaviour UIInternalEvent UIInternalState),
    _ui_ev_in :: !(PushEvent InputEvent),
    _ui_time :: !(SetBehaviour DiffTime),
    _ui_err_bus :: !(MessageBus TolimanGraphicalError)}

makeUnderscoreFields ''UIState'


type UIState = Maybe UIState'

uiStateDefault :: UIState
uiStateDefault = Nothing
