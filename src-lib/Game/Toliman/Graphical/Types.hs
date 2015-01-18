{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Types where

import Control.Monad.Lift.IO (MonadIO)

import Monad.Mask (MonadMask)

import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Rendering.Types (RendererState,
                                               rendererStateDefault)
import Game.Toliman.Graphical.SDL.Types (SDLState,
                                         sdlStateDefault)
import Game.Toliman.Graphical.UI.Types(UIState,
                                       uiStateDefault)
import Monad.Ref (MonadRef(..))


data GraphicalState = GraphicalState {
    _gr_renderer :: !RendererState,
    _gr_sdl :: !SDLState,
    _gr_ui :: !UIState }

makeUnderscoreFields ''GraphicalState

graphicalStateDefault :: GraphicalState
graphicalStateDefault = GraphicalState {
  _gr_renderer = rendererStateDefault,
  _gr_sdl = sdlStateDefault,
  _gr_ui = uiStateDefault}

type MonadGraphical a =
  forall m.
  (MonadGraphicalError m,
   MonadRef GraphicalState m,
   MonadMask m,
   MonadIO m,
   Functor m,
   Applicative m) => m a
