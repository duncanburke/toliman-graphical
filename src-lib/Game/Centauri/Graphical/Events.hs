{-# LANGUAGE NamedFieldPuns #-}
module Game.Centauri.Graphical.Events
       (initEvents,
        finaliseEvents,
        dispatchEvents,
        EventConfig(..),
        EventState) where

import Game.Centauri.Graphical.SDL as SDL

import Control.Exception.Assert
import Control.Monad.State
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

eventbufferlen :: Int
eventbufferlen = 32

data EventState = EventState {
  eventbuffer :: Ptr SDL.Event,
  startexit :: Bool,
  winstatechanged :: Bool }

data EventConfig = EventConfig deriving (Show)

checkPtr_ :: (Storable a) => Ptr a -> Ptr a
checkPtr_ p = assert (p /= nullPtr) p

checkPtr = return . checkPtr_


initEvents :: EventConfig -> IO EventState
initEvents cfg = do
  eventbuffer_ <- mallocArray $ fromIntegral eventbufferlen
  checkPtr eventbuffer_
  return EventState {eventbuffer = eventbuffer_,
                     startexit = False,
                     winstatechanged = False}


finaliseEvents :: EventConfig -> EventState -> IO ()
finaliseEvents cfg st = do
  checkPtr $ eventbuffer st
  free $ eventbuffer st

dispatchEvents :: EventConfig -> EventState -> IO EventState
dispatchEvents cfg st = do
  pumpEvents
  checkPtr $ eventbuffer st
  execStateT (dispatchAll cfg) st


dispatchAll :: EventConfig -> StateT EventState IO ()
dispatchAll cfg = do
  st <- get
  let evbuf = eventbuffer st
  n <- lift $ checkRet "peep events" =<<
       peepEvents evbuf (fromIntegral eventbufferlen) eventActionGetEvent eventTypeFirstEvent eventTypeLastEvent
  foldr (>>) (lift $ return ()) [
    do ev <- lift $ peekElemOff evbuf $ fromIntegral i
       dispatchEvent cfg ev
    | i <- [0..(n-1)]]
  case n of
    0 -> lift $ return ()
    _ -> dispatchAll cfg

dispatchEvent :: EventConfig -> SDL.Event -> StateT EventState IO ()
dispatchEvent _ _ = lift $ return ()

dispatchEvent _ QuitEvent {} = modify $ \evstate -> evstate {startexit = True}

dispatchEvent _ ev@(WindowEvent {eventType}) = do
  evstate <- get
  case eventType of
    windowEventShown -> put $ evstate {winstatechanged = True}
    windowEventHidden -> put $ evstate {winstatechanged = True}
    windowEventRestored -> put $ evstate {winstatechanged = True}
    windowEventEnter -> put $ evstate {winstatechanged = True}
    windowEventLeave -> put $ evstate {winstatechanged = True}
    windowEventFocusGained -> put $ evstate {winstatechanged = True}
    windowEventFocusLost -> put $ evstate {winstatechanged = True}
    windowEventClose -> put $ evstate {startexit = True}
    _ -> lift $ return ()

dispatchEvent _ ev@(KeyboardEvent {eventType}) = do
  evstate <- get
  case eventType of
    eventTypeKeyDown -> lift $ return ()
    eventTypeKeyUp -> lift $ return ()
    _ -> lift $ return ()

dispatchEvent _ ev@(TextInputEvent {eventType}) = do
  evstate <- get
  case eventType of
    eventTypeTextInput -> lift $ return ()
    _ -> lift $ return ()

dispatchEvent _ ev@(MouseMotionEvent {eventType}) = do
  evstate <- get
  case eventType of
    eventTypeMouseMotion -> lift $ return ()
    _ -> lift $ return ()

dispatchEvent _ ev@(MouseButtonEvent {eventType}) = do
  evstate <- get
  case eventType of
    eventTypeMouseButtonDown -> lift $ return ()
    eventTypeMouseButtonUp -> lift $ return ()
    _ -> lift $ return ()

dispatchEvent _ ev@(MouseWheelEvent {eventType}) = do
  evstate <- get
  case eventType of
    eventTypeMouseWheel -> lift $ return ()
    _ -> lift $ return ()
