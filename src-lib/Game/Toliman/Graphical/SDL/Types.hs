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
    category :: LogCategory,
    priority :: LogPriority,
    message :: LogMessage }
  deriving (Show)

makeUnderscoreFields ''LogEntry

type LogBuffer = BankersDequeue LogEntry

type MonadLog = MonadState LogBuffer

