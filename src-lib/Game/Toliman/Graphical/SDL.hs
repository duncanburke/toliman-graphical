{-# LANGUAGE RankNTypes #-}
module Game.Toliman.Graphical.SDL where

import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Control.Monad.IO.Class (MonadIO)

import Graphics.UI.SDL

logMessage' :: (MonadIO m) => CInt -> LogPriority -> CString -> m ()
logMessage' = logMessage

type LogType = (MonadIO m) => CString -> m ()

logDebugApplication :: LogType
logDebugAudio :: LogType
logDebugError :: LogType
logDebugInput :: LogType
logDebugRender :: LogType
logDebugSystem :: LogType
logDebugVideo :: LogType
logInfoApplication :: LogType
logInfoAudio :: LogType
logInfoError :: LogType
logInfoInput :: LogType
logInfoRender :: LogType
logInfoSystem :: LogType
logInfoVideo :: LogType
logWarnApplication :: LogType
logWarnAudio :: LogType
logWarnError :: LogType
logWarnInput :: LogType
logWarnRender :: LogType
logWarnSystem :: LogType
logWarnVideo :: LogType
logErrorApplication :: LogType
logErrorAudio :: LogType
logErrorError :: LogType
logErrorInput :: LogType
logErrorRender :: LogType
logErrorSystem :: LogType
logErrorVideo :: LogType

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


checkRet :: (Monad m) => String -> CInt -> m CInt
checkRet desc ret = case ret of
  -1 -> fail desc
  _ -> return ret



-- checkRet :: (MonadError String m, MonadWriter LogMessage m) => String -> CInt -> m CInt
-- checkRet desc ret = case ret of
--   -1 -> logErrorApplication (printf "%s" desc) >>= throwError
--   _ -> logDebugApplication  (printf "success %s" desc) >> return ret

-- checkError :: (MonadWriter LogMessage m, MonadError String m, MonadIO m) => String -> m a -> m a
-- checkError desc call = do
--   liftIO clearError
--   a <- call
--   err <- liftIO getError
--   c <- liftIO $ peek err
--   case c of
--     0 -> logDebugApplication (printf "success %s" desc) >> return a
--     _ -> do
--       s <- peekCString err
--       logErrorApplication (printf "%s (%s)" desc s) >>= throwError

