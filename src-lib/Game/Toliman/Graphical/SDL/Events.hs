module Game.Toliman.Graphical.SDL.Events where

import Data.Functor ((<$>))
import Foreign.Storable (peekElemOff)

import Control.Monad.Lift.IO (liftIO)
import Graphics.UI.SDL as SDL
  (Event,
   pumpEvents, peepEvents,
   pattern SDL_GETEVENT,
   pattern SDL_FIRSTEVENT, pattern SDL_LASTEVENT)

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.SDL.Types
import Game.Toliman.Graphical.Types (MonadGraphical, sdl)
import Game.Toliman.Graphical.SDL.Core

getEvents :: MonadGraphical [SDL.Event]
getEvents = do
  p <- access (sdl.init_events)
  sdlCheckPred "init_events" p
  liftIO $ pumpEvents
  getEvents'
  where
    getEvents' :: MonadGraphical [SDL.Event]
    getEvents' = do
      b <- access (sdl.ev_buf)
      n <- sdlCheckRet' "peep events" $
           peepEvents b sdlEvBufLen SDL_GETEVENT SDL_FIRSTEVENT SDL_LASTEVENT
      case n of
       0 -> return []
       _ -> (++) <$> getEvents' <*> (liftIO $ sequence [peekElemOff b i | i <- [0..(sdlEvBufLen-1)]])
