{-# LANGUAGE RecursiveDo #-}

module Game.Toliman.Graphical.UI.Events where

import Data.Functor ((<$>))
import Data.Time.Clock (DiffTime)

import qualified Graphics.UI.SDL as SDL

import Game.Toliman.Internal.Sodium as Sodium
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

feedSDLEvents :: [SDL.Event] -> UIState' -> Reactive ()
feedSDLEvents = feedEvents . (SDLEvent <$>)

feedEvents :: [InputEvent] -> UIState' -> Reactive ()
feedEvents ev s =  sequence_ $ s^.ev_in.push <$> ev

dispatchInput :: UIState' -> Reactive ()
dispatchInput s = do
  _ <- execute <$> collectE (inputLoop s) inputStateDefault (s^.ev_in.event)
  return ()

inputLoop :: UIState' -> InputEvent -> InputState -> (Reactive (), InputState)

inputLoop st (SDLEvent (SDL.MouseMotionEvent {..})) s = (return (), s)

inputLoop st (SDLEvent (SDL.MouseButtonEvent {..})) s = (return (), s)

inputLoop st (SDLEvent (SDL.KeyboardEvent {..})) s = (return (), s)

inputLoop st (SDLEvent (SDL.WindowEvent {..})) s = (return (), s)

inputLoop _ (SDLEvent _) s = (return (), s)
