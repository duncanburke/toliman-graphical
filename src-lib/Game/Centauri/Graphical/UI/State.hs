module Game.Centauri.Graphical.UI.State
       (UIState(..),
        UIInputState(..),
        UIEngineCommand(..),
        initUIState,
        finalUIState,
        refreshInputState,
        uiStartCapture,
        uiEndCapture,
        initWidgets,
        finalWidgets,
        WidgetState(..)
        ) where

import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Graphical.UI.Configuration
import Game.Centauri.Core as Core

import Data.Word
import Data.RTree as RTree
import Data.Typeable
import Control.Monad.State as State
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types

data ScreenCoord = ScreenCoord Int Int deriving (Show)

data UIState = UIState {
  uicfg :: UIConfig,
  input_state :: UIInputState,
  widgets :: WidgetState,
  engine_command :: UIEngineCommand
  } deriving (Show)

data UIInputState = UIInputState {
  window_flags :: Word32,
  mouse_state :: Word32,
  mouse_coords :: (Ptr CInt, Ptr CInt)
  } deriving (Show)

data UIEngineCommand =
  EngineNoCommand |
  EngineShutdown |
  EngineReInit deriving (Show)

initUIState :: UIConfig -> IO UIState
initUIState cfg = do
  m0 <- checkPtr =<< malloc
  m1 <- checkPtr =<< malloc
  let input_state_ = UIInputState {window_flags = 0,
                                  mouse_state = 0,
                                  mouse_coords = (m0,m1)}
      engine_command_ = EngineNoCommand
  widgets_ <- initWidgets cfg
  return UIState {uicfg = cfg,
                  input_state = input_state_,
                  widgets = widgets_,
                  engine_command = engine_command_}

finalUIState :: UIState -> IO ()
finalUIState uistate = do
  finalWidgets $ widgets uistate
  let (m0,m1) = mouse_coords $ input_state uistate
  free m0
  free m1


type WidgetID = Int

data WidgetState = WidgetState {
  mouse_input_state :: WidgetMouseState } deriving (Show)

data WidgetMouseState =
  MouseInactive ScreenCoord |
  MousePressed {
    mouse_start :: ScreenCoord,
    mouse_widget_start :: WidgetID,
    mouse_pos :: ScreenCoord,
    mouse_button :: MouseButton } deriving (Show)

data MouseButton =
  MouseLeft |
  MouseRight deriving (Show)
data MouseEvent = MouseEnter |
                  MouseLeave |
                  MouseMove ScreenCoord |
                  MouseDown ScreenCoord MouseButton |
                  MouseUp ScreenCoord MouseButton |
                  MouseClick MouseButton deriving (Show)

data Key = Key deriving (Show)
data KeyboardEvent =
  KeyDown Key |
  KeyUp Key |
  KeyPress Key deriving (Show)

data WidgetEvent =
  KeyboardEvent |
  MouseEvent deriving (Show)

class (Typeable a) => Widget a where
  id :: a -> WidgetID
  pos :: a -> ScreenCoord
  width :: a -> Int
  height :: a -> Int
  active :: a -> Bool
  bb :: a -> MBB
  bb a = mbb (fromIntegral x) (fromIntegral y) (fromIntegral $ x + width a) (fromIntegral $ y + height a)
         where ScreenCoord x y = pos a
  eventhandler :: a -> WidgetEvent -> GameState -> State UIState ()

initWidgets :: UIConfig -> IO WidgetState
initWidgets cfg = do
  mouse_input_state_ <- return $ MouseInactive $ ScreenCoord 0 0
  return WidgetState {
    mouse_input_state = mouse_input_state_ }

finalWidgets :: WidgetState -> IO ()
finalWidgets st = do
  return ()

refreshInputState :: SDL.Window -> UIState -> IO UIState
refreshInputState window uistate = do
  winfl <- SDL.checkError "window flags" $ SDL.getWindowFlags window
  mousest <- SDL.checkError "mouse flags" $ uncurry SDL.getMouseState $ mouse_coords $ input_state uistate
  return uistate {input_state = (input_state uistate) {
                     window_flags = winfl,
                     mouse_state = mousest}}

uiStartCapture :: UIState -> IO UIState
uiStartCapture uistate = do
  let cfg = uicfg uistate
  SDL.checkError "mouse relative" $ SDL.setRelativeMouseMode $ mouse_relative cfg
  return uistate

uiEndCapture :: UIState -> IO UIState
uiEndCapture uistate = do
  SDL.checkError "mouse relative" $ SDL.setRelativeMouseMode False
  return uistate
