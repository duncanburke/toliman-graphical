{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Types where

import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Control.Monad.Lift.IO (MonadIO)
import Monad.Mask (MonadMask)
import Monad.Ref (MonadRef)
import System.Time.Monotonic as Monotonic (Clock)

import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Graphical.Internal.Errors

import Game.Toliman.Graphical.Rendering.Types (
  RendererState, rendererStateDefault)
import Game.Toliman.Graphical.SDL.Types (
  SDLState, sdlStateDefault)
import Game.Toliman.Graphical.UI.Types(
  UIState, uiStateDefault)

data GraphicalState = GraphicalState {
    _gr_renderer :: !RendererState,
    _gr_sdl :: !SDLState,
    _gr_ui :: !UIState,
    _gr_clock :: !(Maybe Monotonic.Clock),
    _gr_time :: !DiffTime }

makeUnderscoreFields ''GraphicalState

graphicalStateDefault :: GraphicalState
graphicalStateDefault = GraphicalState {
  _gr_renderer = rendererStateDefault,
  _gr_sdl = sdlStateDefault,
  _gr_ui = uiStateDefault,
  _gr_clock = Nothing,
  _gr_time = secondsToDiffTime 0}

type MonadGraphical a =
  forall m.
  (MonadGraphicalError m,
   MonadRef GraphicalState m,
   MonadMask m,
   MonadIO m,
   Functor m,
   Applicative m) => m a
