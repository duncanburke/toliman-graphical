
module Game.Toliman.Graphical.UI.Events (
  processEvents,
  translateSDLEvent) where

import Graphics.UI.SDL as SDL (
  Event(..),
  pattern SDL_KEYDOWN
  )

import Game.Toliman.Internal.Sodium
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

processEvents :: [InputEvent] -> MonadUI ()
processEvents ev = do
  e <- access input_channel
  sync $ sequence_ $ map (e ^. push) ev

translateSDLEvent :: SDL.Event -> InputEvent
translateSDLEvent = SDLEvent
