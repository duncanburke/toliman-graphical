{-# LANGUAGE ImpredicativeTypes #-}

module Game.Toliman.Graphical.SDL.Log where

import Prelude hiding (error)

import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Control.Lens

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Dequeue (empty, pushBack, popFront)
import Monad.State (modify)
import Graphics.UI.SDL

import Game.Toliman.Graphical.SDL.Types

logMessage' :: (MonadIO m) => LogCategory -> LogPriority -> LogMessage -> m ()
logMessage' c p m = liftIO $ withCString m $ logMessage c p

pureLogMessage :: (MonadLog m) => LogCategory -> LogPriority -> LogMessage -> m ()
pureLogMessage category priority message = modify $ flip pushBack LogEntry {..}

pureLogFlush :: (MonadIO m) => LogBuffer -> m ()
pureLogFlush l =
  let (entry, l') = popFront l in
  case entry of
   Just LogEntry {..} -> do
     logMessage' category priority message
     pureLogFlush l'
   Nothing -> return ()

runPureLog :: (MonadIO m) => StateT LogBuffer m a -> m a
runPureLog st = do
  (a, buf) <- runStateT st empty
  pureLogFlush buf
  return a

type LogFnType = forall m. (MonadIO m) => String -> m ()
logDebugApplication :: LogFnType
logDebugAudio :: LogFnType
logDebugError :: LogFnType
logDebugInput :: LogFnType
logDebugRender :: LogFnType
logDebugSystem :: LogFnType
logDebugVideo :: LogFnType
logInfoApplication :: LogFnType
logInfoAudio :: LogFnType
logInfoError :: LogFnType
logInfoInput :: LogFnType
logInfoRender :: LogFnType
logInfoSystem :: LogFnType
logInfoVideo :: LogFnType
logWarnApplication :: LogFnType
logWarnAudio :: LogFnType
logWarnError :: LogFnType
logWarnInput :: LogFnType
logWarnRender :: LogFnType
logWarnSystem :: LogFnType
logWarnVideo :: LogFnType
logErrorApplication :: LogFnType
logErrorAudio :: LogFnType
logErrorError :: LogFnType
logErrorInput :: LogFnType
logErrorRender :: LogFnType
logErrorSystem :: LogFnType
logErrorVideo :: LogFnType

logDebugApplication = logMessage' SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_DEBUG
logDebugAudio = logMessage' SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_DEBUG
logDebugError = logMessage' SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_DEBUG
logDebugInput = logMessage' SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_DEBUG
logDebugRender = logMessage' SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_DEBUG
logDebugSystem = logMessage' SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_DEBUG
logDebugVideo = logMessage' SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_DEBUG
logInfoApplication = logMessage' SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_INFO
logInfoAudio = logMessage' SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_INFO
logInfoError = logMessage' SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_INFO
logInfoInput = logMessage' SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_INFO
logInfoRender = logMessage' SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_INFO
logInfoSystem = logMessage' SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_INFO
logInfoVideo = logMessage' SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_INFO
logWarnApplication = logMessage' SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_WARN
logWarnAudio = logMessage' SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_WARN
logWarnError = logMessage' SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_WARN
logWarnInput = logMessage' SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_WARN
logWarnRender = logMessage' SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_WARN
logWarnSystem = logMessage' SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_WARN
logWarnVideo = logMessage' SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_WARN
logErrorApplication = logMessage' SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_ERROR
logErrorAudio = logMessage' SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_ERROR
logErrorError = logMessage' SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_ERROR
logErrorInput = logMessage' SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_ERROR
logErrorRender = logMessage' SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_ERROR
logErrorSystem = logMessage' SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_ERROR
logErrorVideo = logMessage' SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_ERROR

type PureLogFnType = forall m. (MonadLog m) => String -> m ()
logPureDebugApplication :: PureLogFnType
logPureDebugAudio :: PureLogFnType
logPureDebugError :: PureLogFnType
logPureDebugInput :: PureLogFnType
logPureDebugRender :: PureLogFnType
logPureDebugSystem :: PureLogFnType
logPureDebugVideo :: PureLogFnType
logPureInfoApplication :: PureLogFnType
logPureInfoAudio :: PureLogFnType
logPureInfoError :: PureLogFnType
logPureInfoInput :: PureLogFnType
logPureInfoRender :: PureLogFnType
logPureInfoSystem :: PureLogFnType
logPureInfoVideo :: PureLogFnType
logPureWarnApplication :: PureLogFnType
logPureWarnAudio :: PureLogFnType
logPureWarnError :: PureLogFnType
logPureWarnInput :: PureLogFnType
logPureWarnRender :: PureLogFnType
logPureWarnSystem :: PureLogFnType
logPureWarnVideo :: PureLogFnType
logPureErrorApplication :: PureLogFnType
logPureErrorAudio :: PureLogFnType
logPureErrorError :: PureLogFnType
logPureErrorInput :: PureLogFnType
logPureErrorRender :: PureLogFnType
logPureErrorSystem :: PureLogFnType
logPureErrorVideo :: PureLogFnType

logPureDebugApplication = pureLogMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_DEBUG
logPureDebugAudio = pureLogMessage SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_DEBUG
logPureDebugError = pureLogMessage SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_DEBUG
logPureDebugInput = pureLogMessage SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_DEBUG
logPureDebugRender = pureLogMessage SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_DEBUG
logPureDebugSystem = pureLogMessage SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_DEBUG
logPureDebugVideo = pureLogMessage SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_DEBUG
logPureInfoApplication = pureLogMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_INFO
logPureInfoAudio = pureLogMessage SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_INFO
logPureInfoError = pureLogMessage SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_INFO
logPureInfoInput = pureLogMessage SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_INFO
logPureInfoRender = pureLogMessage SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_INFO
logPureInfoSystem = pureLogMessage SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_INFO
logPureInfoVideo = pureLogMessage SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_INFO
logPureWarnApplication = pureLogMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_WARN
logPureWarnAudio = pureLogMessage SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_WARN
logPureWarnError = pureLogMessage SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_WARN
logPureWarnInput = pureLogMessage SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_WARN
logPureWarnRender = pureLogMessage SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_WARN
logPureWarnSystem = pureLogMessage SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_WARN
logPureWarnVideo = pureLogMessage SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_WARN
logPureErrorApplication = pureLogMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_ERROR
logPureErrorAudio = pureLogMessage SDL_LOG_CATEGORY_AUDIO SDL_LOG_PRIORITY_ERROR
logPureErrorError = pureLogMessage SDL_LOG_CATEGORY_ERROR SDL_LOG_PRIORITY_ERROR
logPureErrorInput = pureLogMessage SDL_LOG_CATEGORY_INPUT SDL_LOG_PRIORITY_ERROR
logPureErrorRender = pureLogMessage SDL_LOG_CATEGORY_RENDER SDL_LOG_PRIORITY_ERROR
logPureErrorSystem = pureLogMessage SDL_LOG_CATEGORY_SYSTEM SDL_LOG_PRIORITY_ERROR
logPureErrorVideo = pureLogMessage SDL_LOG_CATEGORY_VIDEO SDL_LOG_PRIORITY_ERROR

logPrioritiesMap :: [(Lens' LogPriorities LogPriority, CInt)]
logPrioritiesMap = [
  (application, SDL_LOG_CATEGORY_APPLICATION),
  (error, SDL_LOG_CATEGORY_APPLICATION),
  (system, SDL_LOG_CATEGORY_SYSTEM),
  (audio, SDL_LOG_CATEGORY_AUDIO),
  (video, SDL_LOG_CATEGORY_VIDEO),
  (render, SDL_LOG_CATEGORY_RENDER),
  (input, SDL_LOG_CATEGORY_INPUT)]

setLogPriorities :: (MonadIO m) => LogPriorities -> m ()
setLogPriorities p =
  liftIO $ sequence_ [logSetPriority c (p ^. l) | (l,c) <- logPrioritiesMap]
