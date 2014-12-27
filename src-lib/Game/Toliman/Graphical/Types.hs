{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Types where

import Control.Monad.Lift.IO (MonadIO)
import Control.Applicative (Applicative)

import Monad.Mask (MonadMask)

import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Rendering.Types (RendererState,
                                               rendererStateDefault)
import Game.Toliman.Graphical.SDL.Types (SDLState,
                                         sdlStateDefault)
import Monad.Ref (MonadRef(..))


data GraphicalState = GraphicalState {
    _gr_renderer :: !RendererState,
    _gr_sdl :: !SDLState}

makeUnderscoreFields ''GraphicalState

graphicalStateDefault :: GraphicalState
graphicalStateDefault = GraphicalState {
  _gr_renderer = rendererStateDefault,
  _gr_sdl = sdlStateDefault }

type MonadGraphical a =
  (MonadGraphicalError m,
   MonadRef GraphicalState m,
   MonadMask m,
   MonadIO m,
   Functor m,
   Applicative m) => m a
