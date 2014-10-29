{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}
{- |
   module      : Game.Centauri.Graphical
   copyright   : (c) Duncan Burke
   license     : MPL
   maintaner   : Duncan Burke <duncankburke@gmail.cmo>
-}

module Game.Centauri.Graphical
       ( graphicalMain ) where

import qualified Game.Centauri.Configuration as Conf
import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Graphical.OpenGL as GL
import Game.Centauri.Graphical.Events

import Control.Exception.BracketM

import Foreign.Ptr
import Control.Applicative
import Control.Monad.State as State
import Foreign.C.String
import Foreign.Marshal.Alloc


data GraphicalState = GraphicalState
                      { window :: SDL.Window,
                        glctx :: SDL.GLContext,
                        evstate :: EventState }

graphicalMain :: Conf.GameConfig -> IO ()
graphicalMain cfg = runBracketM $ do
  lift $ SDL.logSetPriority logCategoryApplication logPriorityDebug
  bracketM_ initSDL finalSDL
  drivers <- lift SDL.getVideoDrivers
  lift $ print drivers
  bracketM_ (initVideo cfg) finalVideo
  evstate_ <- bracketM (initEvents $ Conf.evconfig cfg) (finaliseEvents $ Conf.evconfig cfg)
  win_title <- bracketM (newCString "OpenCentauri") free
  window_ <- bracketM (initWindow cfg win_title) finalWindow
  glctx_ <- bracketM (initGLContext cfg window_) finalGLContext
  let window = window_
      glctx = glctx_
      evstate = evstate_ in
    return $ evalStateT (graphicalLoop cfg) $ GraphicalState {window,glctx,evstate}

graphicalLoop :: Conf.GameConfig -> StateT GraphicalState IO ()
graphicalLoop cfg = do
  evstate_ <- (evstate <$> State.get) >>= \evstate_ -> (lift $ dispatchEvents (Conf.evconfig cfg) evstate_)
  modify (\st -> st {evstate=evstate_})
  doRender cfg
  case False of
    True -> lift $ return ()
    False -> graphicalLoop cfg

doRender :: Conf.GameConfig -> StateT GraphicalState IO ()
doRender cfg = do
  st <- State.get
  lift $ do
    clearColor $= Color4 0 0 1 0
    clear  [ColorBuffer]
    SDL.logDebugVideo "swapping"
    SDL.checkError "swap_window" $ SDL.glSwapWindow (window st)
    SDL.delay 1000


initSDL = do
  SDL.logInfoApplication "sdl init"
  checkRet "sdl_init" =<< SDL.init 0

finalSDL = do
  SDL.logInfoApplication "sdl final"
  SDL.quit

initVideo cfg = do
    SDL.logInfoApplication "video init"
    checkRet "sdl_subsys_video_init" =<< SDL.initSubSystem initFlagVideo
    runBracketM $ do
      driver <- case (Conf.driver cfg) of
        "" -> lift $ return nullPtr
        _ -> bracketM (newCString $ Conf.driver cfg) (free)
      return $ checkRet "video_init" =<< SDL.videoInit driver
    glSetAttrs $ glframebuffer 8 8 8 8 24 8
    glSetAttrs $ gldoublebuffer True
    glSetAttrs $ glaccelerated True
    glSetAttrs $ gldebug True
    glSetAttrs $ glmsaa 0

finalVideo = do
  SDL.logInfoApplication "video final"
  SDL.videoQuit
  SDL.quitSubSystem SDL.initFlagVideo

initWindow :: Conf.GameConfig -> CString -> IO SDL.Window
initWindow cfg title = runBracketM $ do
  lift $ SDL.logInfoApplication "window init"
  let fullscreen = Conf.fullscreen cfg
      borderless = Conf.borderless cfg
      width = Conf.width cfg
      height = Conf.height cfg
      x = SDL.windowPosUndefined
      y = SDL.windowPosUndefined
      flags = glwindowflags fullscreen borderless False False False
  return $ checkError "create_window" $ createWindow title x y width height flags

finalWindow window = do
  logInfoApplication "window final"
  destroyWindow window


initGLContext :: Conf.GameConfig -> SDL.Window -> IO SDL.GLContext
initGLContext cfg window = do
  logInfoApplication "gl ctx init"
  let vsync = Conf.vsync cfg
      late_tear = Conf.vsync_late_tear cfg
      swap_interval = case () of
        _ | vsync && not late_tear -> 1
        _ | vsync && late_tear -> -1
        _ -> 0
  ctx <- checkError "create_gl_context" $ glCreateContext window
  checkRet "gl_set_swap_interval" =<< glSetSwapInterval swap_interval
  return ctx

finalGLContext ctx = do
  logInfoApplication "gl ctx final"
  glDeleteContext ctx
