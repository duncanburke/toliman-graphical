{-# LANGUAGE ImpredicativeTypes #-}

module Game.Toliman.Graphical.Rendering.Window (
  toWindowFlags,
  fromWindowFlags,
  fromWindowPos,
  swapWindow) where

import Data.Bits ((.|.), (.&.))
import Foreign.C.Types (CInt)

import Control.Monad.Lift.IO (liftIO)
import Graphics.UI.SDL
import Control.Lens


import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Types (MonadGraphical, renderer)
import Game.Toliman.Graphical.Rendering.Types


windowFlagsMap :: [(Lens' WindowFlags Bool, SDL_WindowFlags)]
windowFlagsMap = [(fullscreen, SDL_WINDOW_FULLSCREEN),
                  (fullscreen_desktop, SDL_WINDOW_FULLSCREEN_DESKTOP),
                  (opengl, SDL_WINDOW_OPENGL),
                  (hidden, SDL_WINDOW_HIDDEN),
                  (borderless, SDL_WINDOW_BORDERLESS),
                  (resizable, SDL_WINDOW_RESIZABLE),
                  (minimized, SDL_WINDOW_MINIMIZED),
                  (maximized, SDL_WINDOW_MAXIMIZED),
                  (input_grabbed, SDL_WINDOW_INPUT_GRABBED),
                  (allow_highdpi, SDL_WINDOW_ALLOW_HIGHDPI)]

toWindowFlags :: SDL_WindowFlags -> WindowFlags
toWindowFlags fl =
  foldr (.) id [\s -> s & l .~ (not $ mask .&. fl == 0) | (l,mask) <- windowFlagsMap] $
  windowFlagsDefault

fromWindowFlags :: WindowFlags -> SDL_WindowFlags
fromWindowFlags fl =
  foldr (.) id [\s -> s .|. mask | (l,mask) <- windowFlagsMap, fl ^. l] $ 0

fromWindowPos :: (WindowPos, WindowPos) -> (CInt,CInt)
fromWindowPos (x,y) =
  (fromWindowPos' x, fromWindowPos' y)
  where fromWindowPos' :: WindowPos -> CInt
        fromWindowPos' WindowCentred = SDL_WINDOWPOS_CENTERED
        fromWindowPos' WindowUndefined = SDL_WINDOWPOS_UNDEFINED

swapWindow :: MonadGraphical ()
swapWindow = do
  win <- getJust "window" $ accessPrism $ renderer.window._Just.handle
  liftIO $ glSwapWindow win
