{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}

module Game.Centauri.Graphical.UI.Events
       (dispatchEvents) where

import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Core as Core

import Game.Centauri.Graphical.UI.State as State

import Control.Monad.State
import Control.Applicative
import Data.Foldable
import Data.Monoid

instance Monoid (State UIState ()) where
  mempty = return ()
  mappend = (>>)

dispatchEvents :: [SDL.Event] -> GameState -> State UIState ()
dispatchEvents evl gst = fold $ flip dispatchEvent gst <$> evl

type EventHandler = SDL.Event -> Core.GameState -> State UIState ()

dispatchEvent :: EventHandler

dispatchEvent ev@(QuitEvent {})= exitRequested ev

dispatchEvent ev@(WindowEvent {eventType}) =
  case eventType of
    windowEventShown -> winStateChanged ev
    windowEventHidden -> winStateChanged ev
    windowEventRestored -> winStateChanged ev
    windowEventEnter -> winStateChanged ev
    windowEventLeave -> winStateChanged ev
    windowEventFocusGained -> winStateChanged ev
    windowEventFocusLost -> winStateChanged ev
    windowEventClose -> winStateChanged ev
    _ -> \_ -> return ()

-- dispatchEvent _ ev@(MouseMotionEvent {eventType}) = do
--   evstate <- get
--   case eventType of
--     eventTypeMouseMotion -> lift $ return ()
--     _ -> lift $ return ()

-- dispatchEvent _ ev@(MouseButtonEvent {eventType}) = do
--   evstate <- get
--   case eventType of
--     eventTypeMouseButtonDown -> lift $ return ()
--     eventTypeMouseButtonUp -> lift $ return ()
--     _ -> lift $ return ()


-- dispatchEvent ev@(KeyboardEvent {eventType}) = do
--   case eventType of
--     eventTypeKeyDown -> lift $ return ()
--     eventTypeKeyUp -> lift $ return ()
--     _ -> lift $ return ()

-- dispatchEvent _ ev@(TextInputEvent {eventType}) = do
--   evstate <- get
--   case eventType of
--     eventTypeTextInput -> lift $ return ()
--     _ -> lift $ return ()

-- dispatchEvent _ ev@(MouseWheelEvent {eventType}) = do
--   evstate <- get
--   case eventType of
--     eventTypeMouseWheel -> lift $ return ()
--     _ -> lift $ return ()


dispatchEvent _ = \_ -> return ()


winStateChanged :: EventHandler
winStateChanged ev@(WindowEvent {}) _ = do
  return ()


exitRequested :: EventHandler
exitRequested _ _ = modify $ \uistate -> uistate {engine_command = EngineShutdown}


