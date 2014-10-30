module Game.Centauri.Graphical.Events
       (initEvents,
        finalEvents,
        getEvents,
        EventState) where

import Game.Centauri.Graphical.SDL as SDL

import Control.Applicative
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

eventbufferlen :: Int
eventbufferlen = 32

data EventState = EventState {
  eventbuffer :: Ptr SDL.Event }

initEvents :: IO EventState
initEvents = do
  eventbuffer_ <- mallocArray $ fromIntegral eventbufferlen
  checkPtr eventbuffer_
  return EventState {eventbuffer = eventbuffer_}

finalEvents :: EventState -> IO ()
finalEvents st = do
  checkPtr $ eventbuffer st
  free $ eventbuffer st

getEvents :: EventState -> IO [SDL.Event]
getEvents st = do
  pumpEvents
  checkPtr $ eventbuffer st
  getEvents_ st

getEvents_ :: EventState -> IO [SDL.Event]
getEvents_ st = do
  n <- checkRet "peep events" =<<
       peepEvents (eventbuffer st) (fromIntegral eventbufferlen) eventActionGetEvent eventTypeFirstEvent eventTypeLastEvent
  case n of
    0 -> return []
    _ -> (++) <$> getEvents_ st <*>
         foldl' (\b -> (=<<) (\l -> (:)l <$> b)) (return []) [peekElemOff (eventbuffer st) $ fromIntegral i | i <- [0..(n-1)]]
