{-# LANGUAGE ImpredicativeTypes #-}

module Game.Toliman.Graphical.Rendering.Window (
  toWindowFlags,
  fromWindowFlags) where

import Data.Bits ((.|.), (.&.))

import Graphics.UI.SDL
import Control.Lens

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

