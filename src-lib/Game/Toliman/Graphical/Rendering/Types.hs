{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Rendering.Types where

import Data.IORef (IORef)
import Control.Applicative (Applicative)
import Data.Word (Word32)

import qualified Graphics.UI.SDL as SDL (Window, GLContext)
import Monad.Reader (MonadReader)
import Control.Monad.Lift.IO (MonadIO)

import Game.Toliman.Graphical.Internal.Types
import Game.Toliman.Graphical.Internal.Errors

data WindowFlags =
  WindowFlags {
    _wf_fullscreen :: !Bool,
    _wf_fullscreen_desktop :: !Bool,
    _wf_opengl :: !Bool,
    _wf_hidden :: !Bool,
    _wf_borderless :: !Bool,
    _wf_resizable :: !Bool,
    _wf_minimized :: !Bool,
    _wf_maximized :: !Bool,
    _wf_input_grabbed :: !Bool,
    _wf_allow_highdpi :: !Bool }
  deriving (Show)

makeUnderscoreFields ''WindowFlags

windowFlagsDefault :: WindowFlags
windowFlagsDefault = WindowFlags {
  _wf_fullscreen = False,
  _wf_fullscreen_desktop = False,
  _wf_opengl = True,
  _wf_hidden = False,
  _wf_borderless = False,
  _wf_resizable = False,
  _wf_minimized = False,
  _wf_maximized = False,
  _wf_input_grabbed = False,
  _wf_allow_highdpi = False }

type SDL_WindowFlags = Word32

data WindowPos =
  WindowCentred |
  WindowUndefined
  deriving (Show)

data WindowConfig =
  WindowConfig {
    _win_title :: !String,
    _win_pos :: !WindowPos,
    _win_resolution :: !(Int,Int) }
  deriving (Show)

makeUnderscoreFields ''WindowConfig

windowConfigDefault :: WindowConfig
windowConfigDefault = WindowConfig {
  _win_title = "Toliman",
  _win_pos = WindowUndefined,
  _win_resolution = (640,480) }

data Window =
  Window {
    _win_handle :: !SDL.Window,
    _win_config :: !WindowConfig }
  deriving (Show)

makeUnderscoreFields ''Window

data RendererState = RendererState {
  _rd_window :: !(Maybe Window),
  _rd_glctx :: !(Maybe SDL.GLContext) }

makeUnderscoreFields ''RendererState

rendererStateDefault :: RendererState
rendererStateDefault = RendererState {
  _rd_window = Nothing,
  _rd_glctx = Nothing }

type MonadRenderer a = (MonadGraphicalError m, MonadReader (IORef RendererState) m, MonadIO m, Functor m, Applicative m) => m a
