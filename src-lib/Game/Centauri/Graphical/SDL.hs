module Game.Centauri.Graphical.SDL
       ( module Graphics.UI.SDL,
         logDebugApplication,
         logDebugAudio,
         logDebugError,
         logDebugInput,
         logDebugRender,
         logDebugSystem,
         logDebugVideo,
         logInfoApplication,
         logInfoAudio,
         logInfoError,
         logInfoInput,
         logInfoRender,
         logInfoSystem,
         logInfoVideo,
         logWarnApplication,
         logWarnAudio,
         logWarnError,
         logWarnInput,
         logWarnRender,
         logWarnSystem,
         logWarnVideo,
         logErrorApplication,
         logErrorAudio,
         logErrorError,
         logErrorInput,
         logErrorRender,
         logErrorSystem,
         logErrorVideo,
         checkRet,
         checkError,
         getVideoDrivers,
         checkPtr,
         checkPtr_)
       where


import Control.Exception.Assert
import Control.Applicative
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Graphics.UI.SDL
import Text.Printf

logMessage_ :: LogPriority -> CInt -> String -> IO String
logMessage_ pri c s = do
  withCString s $ logMessage c pri
  return s

logDebugApplication = logMessage_ logPriorityDebug logCategoryApplication
logDebugAudio = logMessage_ logPriorityDebug logCategoryAudio
logDebugError = logMessage_ logPriorityDebug logCategoryError
logDebugInput = logMessage_ logPriorityDebug logCategoryInput
logDebugRender = logMessage_ logPriorityDebug logCategoryRender
logDebugSystem = logMessage_ logPriorityDebug logCategorySystem
logDebugVideo = logMessage_ logPriorityDebug logCategoryVideo

logInfoApplication = logMessage_ logPriorityInfo logCategoryApplication
logInfoAudio = logMessage_ logPriorityInfo logCategoryAudio
logInfoError = logMessage_ logPriorityInfo logCategoryError
logInfoInput = logMessage_ logPriorityInfo logCategoryInput
logInfoRender = logMessage_ logPriorityInfo logCategoryRender
logInfoSystem = logMessage_ logPriorityInfo logCategorySystem
logInfoVideo = logMessage_ logPriorityInfo logCategoryVideo

logWarnApplication = logMessage_ logPriorityWarn logCategoryApplication
logWarnAudio = logMessage_ logPriorityWarn logCategoryAudio
logWarnError = logMessage_ logPriorityWarn logCategoryError
logWarnInput = logMessage_ logPriorityWarn logCategoryInput
logWarnRender = logMessage_ logPriorityWarn logCategoryRender
logWarnSystem = logMessage_ logPriorityWarn logCategorySystem
logWarnVideo = logMessage_ logPriorityWarn logCategoryVideo

logErrorApplication = logMessage_ logPriorityError logCategoryApplication
logErrorAudio = logMessage_ logPriorityError logCategoryAudio
logErrorError = logMessage_ logPriorityError logCategoryError
logErrorInput = logMessage_ logPriorityError logCategoryInput
logErrorRender = logMessage_ logPriorityError logCategoryRender
logErrorSystem = logMessage_ logPriorityError logCategorySystem
logErrorVideo = logMessage_ logPriorityError logCategoryVideo

checkRet :: String -> CInt -> IO CInt
checkRet desc ret = case ret of
  -1 -> (logErrorApplication $ printf "%s" desc) >>= fail
  _ -> (logDebugApplication $ printf "success %s" desc) >> return ret

checkError :: String -> IO a -> IO a
checkError desc call = do
  clearError
  a <- call
  err <- getError
  c <- peek err
  case c of
    0 -> (logDebugApplication $ printf "success %s" desc) >> return a
    _ -> do
      s <- peekCString err
      (logErrorApplication $ printf "%s (%s)" desc s) >>= fail

getVideoDrivers :: IO [String]
getVideoDrivers = do
  n_drivers <- checkRet "num_video_drivers" =<< getNumVideoDrivers
  sequence $ (\n -> (getVideoDriver n) >>= peekCString) <$> [0..(n_drivers-1)]

checkPtr_ :: (Storable a) => Ptr a -> Ptr a
checkPtr_ p = assert (p /= nullPtr) p

checkPtr :: (Storable a) => Ptr a -> IO (Ptr a)
checkPtr = return . checkPtr_
