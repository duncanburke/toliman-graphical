{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.SDL.Types where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)

import Data.Dequeue
import Graphics.UI.SDL as SDL
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

sdlEvBufLen :: (Integral a) => a
sdlEvBufLen = 64

data SDLState =
  SDLState {
    _sdl_init_sdl :: !Bool,
    _sdl_init_video :: !Bool,
    _sdl_init_events :: !Bool,
    _sdl_ev_buf :: !(Ptr SDL.Event) }
  deriving (Show)

makeUnderscoreFields ''SDLState

sdlStateDefault :: SDLState
sdlStateDefault = SDLState {
  _sdl_init_sdl = False,
  _sdl_init_video = False,
  _sdl_init_events = False,
  _sdl_ev_buf = nullPtr }

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
