{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.SDL.Types where

import Foreign.C.Types (CInt)

import Data.Dequeue
import Graphics.UI.SDL
import Monad.State (MonadState)

import Game.Toliman.Graphical.Internal.Types

type LogCategory = CInt
type LogMessage = String
data LogEntry =
  LogEntry {
    category :: !LogCategory,
    priority :: !LogPriority,
    message :: !LogMessage }
  deriving (Show)

makeUnderscoreFields ''LogEntry

type LogBuffer = BankersDequeue LogEntry

type MonadLog = MonadState LogBuffer

data SDLState = SDLState {
  _sdl_init_sdl :: !Bool,
  _sdl_init_video :: !Bool }
  deriving (Show)

makeUnderscoreFields ''SDLState

sdlStateDefault :: SDLState
sdlStateDefault = SDLState {
  _sdl_init_sdl = False,
  _sdl_init_video = False }

data LogPriorities =
  LogPriorities {
    _log_application :: LogPriority,
    _log_error :: LogPriority,
    _log_system :: LogPriority,
    _log_audio :: LogPriority,
    _log_video :: LogPriority,
    _log_render :: LogPriority,
    _log_input :: LogPriority }
  deriving (Show)

makeUnderscoreFields ''LogPriorities

logPrioritiesUniform :: LogPriority -> LogPriorities
logPrioritiesUniform pri = LogPriorities {
  _log_application = pri,
  _log_error = pri,
  _log_system = pri,
  _log_audio = pri,
  _log_video = pri,
  _log_render = pri,
  _log_input = pri }
