{-# LANGUAGE ImpredicativeTypes #-}
module Game.Centauri.Graphical.UI.State
       (UIState(..),
        UIEngineCommand(..),
        UIInputState(..),
        UIElem,
        UIMouseButton(..),
        UIMouseEvent(..),
        UIKeyboardEvent(..),
        UIInputEvent(..),
        UIInputEventStatus(..),
        uiElemSendEvent,
        uiElemDraw,
        uiElemModify,
        initUIState,
        finalUIState,
        uiStartCaptureInput,
        uiEndCaptureInput,
        uiSendEngineCommand,
        refreshInputState,
        uiMouseMotionEvent,
        uiMouseButtonEvent,
        uiKeyboardEvent,
        uiTextInputEvent,
        uiQuitEvent
        ) where

import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Graphical.UI.Configuration
import Game.Centauri.Graphical.Rendering as Rendering

import Data.Map.Strict as Map
import Data.RTree as RTree
import Data.Set as Set
import Data.Tuple.Curry
import Control.Monad.State as State
import Control.Applicative
import Text.Printf

type ScreenCoord = (Int,Int)

data UIState = UIState {
  uicfg :: UIConfig,
  sdl_input_state :: SDLInputState,
  input_state :: UIInputState,
  input_elems :: UIInputElems,
  engine_command :: UIEngineCommand
  } deriving (Show)

data UIEngineCommand =
  EngineNoCommand |
  EngineShutdown |
  EngineReInit deriving (Show)

data UIInputState = UIInputState {
  mouse_state :: UIMouseState,
  keyboard_state :: UIKeyboardState } deriving (Show)

type UIElemID = Int

data UIInputElems = UIInputElems {
  elem_map :: Map UIElemID UIElem }

instance Show UIInputElems where
  show el = printf "Elems { elem_map = Map (%d) }" (Map.size $ elem_map el)

type UIElem = forall a. UIElem_ a

data UIElem_ a = UIElem {
  elem_id :: UIElemID,
  elem_active :: Bool,
  elem_pos :: ScreenCoord,
  elem_size :: ScreenCoord,
  elem_parent :: Maybe UIElemID,
  elem_children :: Set UIElemID,
  _this :: a,
  _handle_event :: a -> UIInputEvent -> UIState -> State UIState UIInputEventStatus,
  _draw :: a -> RenderCommand }

instance Show (UIElem_ a) where
  show el = uncurryN (printf "Elem(%s,%s,(%s,%s),(%s,%s))") t
            where t = (elem_id$el,
                       show.elem_active$el,
                       fst.elem_pos$el,
                       snd.elem_pos$el,
                       fst.elem_size$el,
                       snd.elem_size$el)

uiElemSendEvent UIElem {_this = this, _handle_event = handle_event} = handle_event this
uiElemDraw UIElem {_this = this, _draw = draw} = draw this

uiElemModify :: forall a. UIElem_ a -> a -> UIElem_ a
uiElemModify el this = el {_this = this}

uiElemBB :: UIElem -> MBB
uiElemBB el = mbb x y (x+width) (y+height)
  where [x,y,width,height] = fromIntegral <$>
                             [fst.elem_pos$el,
                              snd.elem_pos$el,
                              fst.elem_size$el,
                              snd.elem_size$el]

data UIMouseState =
  MouseInactive ScreenCoord |
  MousePressed {
    mouse_start :: ScreenCoord,
    mouse_start_elem :: UIElemID,
    mouse_pos :: ScreenCoord,
    mouse_button :: UIMouseButton } deriving (Show)

data UIKeyboardFocus =
  KeyboardNoFocus |
  KeyboardElemFocus UIElemID deriving (Show)

data UIKeyboardState = UIKeyboardState {
  keyboard_focus :: UIKeyboardFocus } deriving (Show)

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

data UIKeyboardEvent =
  KeyDown Keysym |
  KeyUp Keysym |
  KeyPress Keysym deriving (Show)

data UIInputEvent =
  UIKeyboardEvent |
  UIMouseEvent deriving (Show)

data UIInputEventStatus =
  EventHandled |
  EventFallThrough deriving (Show)

initUIState :: UIConfig -> IO UIState
initUIState cfg = do
  sdl_input_state_ <- initSDLInputState
  input_state_ <- initUIInputState cfg
  input_elems_ <- initUIElems cfg
  return UIState {uicfg = cfg,
                  sdl_input_state = sdl_input_state_,
                  input_state = input_state_,
                  input_elems = input_elems_,
                  engine_command = EngineNoCommand}

finalUIState :: UIState -> IO ()
finalUIState st = do
  finalSDLInputState $ sdl_input_state st
  finalUIInputState $ input_state st
  finalUIElems $ input_elems st

initUIInputState :: UIConfig -> IO UIInputState
initUIInputState cfg = do
  let mouse_state_ = MouseInactive (0,0)
      keyboard_state_ = UIKeyboardState KeyboardNoFocus
  return UIInputState {
    mouse_state = mouse_state_,
    keyboard_state = keyboard_state_ }

finalUIInputState :: UIInputState -> IO ()
finalUIInputState st = return ()

initUIElems :: UIConfig -> IO UIInputElems
initUIElems cfg = do
  let elem_map_ = Map.empty
  return UIInputElems {
    elem_map = elem_map_ }

finalUIElems :: UIInputElems -> IO ()
finalUIElems el = return ()

uiStartCaptureInput :: UIState -> IO UIState
uiStartCaptureInput uistate = do
  let cfg = uicfg uistate
  SDL.checkError "mouse relative" $ SDL.setRelativeMouseMode $ mouse_relative cfg
  return uistate

uiEndCaptureInput :: UIState -> IO UIState
uiEndCaptureInput uistate = do
  SDL.checkError "mouse relative" $ SDL.setRelativeMouseMode False
  return uistate

uiSendEngineCommand :: UIEngineCommand -> State UIState ()
uiSendEngineCommand cmd =
  modify (
    \st -> case cmd of
      EngineShutdown -> st {engine_command = EngineShutdown}
      EngineReInit -> case engine_command st of
        EngineShutdown -> st
        _ -> st {engine_command = EngineReInit}
      _ -> st)

refreshInputState :: SDL.Window -> StateT UIState IO ()
refreshInputState win = do
  st <- State.get
  lift (reloadSDLInputState win (sdl_input_state st)) >>=
   \sdl_st -> put st{sdl_input_state = sdl_st}

uiMouseMotionEvent :: Int -> Int -> Int -> Int -> State UIState ()
uiMouseMotionEvent x y xrel yrel = return ()

uiMouseButtonEvent :: UIMouseButton -> Bool -> Int -> Int -> Int -> State UIState ()
uiMouseButtonEvent button isdown clicks x y = return ()

uiKeyboardEvent :: Keysym -> Bool -> Bool -> State UIState ()
uiKeyboardEvent keysym isdown isrepeat = return ()

uiTextInputEvent :: String -> State UIState ()
uiTextInputEvent str = return ()

uiQuitEvent :: State UIState ()
uiQuitEvent = return ()
