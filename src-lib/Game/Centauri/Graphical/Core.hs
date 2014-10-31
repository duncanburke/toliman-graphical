module Game.Centauri.Graphical.Core
       (graphicalMain) where

import Game.Centauri.Configuration as Conf
import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Graphical.OpenGL as GL
import Game.Centauri.Graphical.Events as Events
import Game.Centauri.Graphical.UI as UI
import Game.Centauri.Graphical.Configuration
import Game.Centauri.Core as Core
import Game.Centauri.Assets

import Control.Exception.BracketM

import Foreign.Ptr
import Control.Monad.State as State
import Foreign.C.String
import Foreign.Marshal.Alloc


data GraphicalState = GraphicalState
                      { window :: SDL.Window,
                        glctx :: SDL.GLContext,
                        evstate :: Events.EventState,
                        uistate :: UI.UIState,
                        gstate :: Core.GameState,
                        astate :: AssetState}

graphicalMain :: GameConfig -> IO ()
graphicalMain GameConfig {asset_config,ui_config,graphics_config} =
  runBracketM $ do
    bracketM_ (initSDL graphics_config) finalSDL
    bracketM_ (initVideo graphics_config) finalVideo
    evstate_ <- bracketM initEvents finalEvents
    uistate_ <- bracketM (initUIState ui_config) finalUIState
    gstate_ <- bracketM initGameState finalGameState
    astate_ <- bracketM (initAssetState asset_config) finalAssetState
    win_title <- bracketM (newCString "OpenCentauri") free
    window_ <- bracketM (initWindow graphics_config win_title) finalWindow
    glctx_ <- bracketM (initGLContext graphics_config window_) finalGLContext
    return $ evalStateT graphicalLoop GraphicalState {
      window = window_,
      glctx = glctx_,
      evstate = evstate_,
      uistate = uistate_,
      gstate = gstate_,
      astate = astate_}

graphicalLoop :: StateT GraphicalState IO ()
graphicalLoop = do
  State.get >>= \st -> lift $ do
    events <- Events.getEvents $ evstate st
    uistate_ <- execStateT (refreshInputState $ window st) (uistate st)
    uistate_ <- return $ execState (dispatchEvents events) uistate_
    return st { uistate = uistate_ }
  doRender
  case False of
    True -> lift $ return ()
    False -> graphicalLoop

doRender :: StateT GraphicalState IO ()
doRender = do
  st <- State.get
  lift $ do
    clearColor $= Color4 0 0 1 0
    clear  [ColorBuffer]
    SDL.logDebugVideo "swapping"
    SDL.checkError "swap_window" $ SDL.glSwapWindow (window st)
    SDL.delay 1000

initSDL :: GraphicsConfig -> IO ()
initSDL cfg = do
  SDL.logInfoApplication "sdl init"
  checkRet "sdl_init" =<< SDL.init 0
  sequence_ [ checkError "sdl_log_pri" $ SDL.logSetPriority cat pri | (cat,pri) <- sdl_log_pri cfg]

finalSDL :: IO ()
finalSDL = do
  SDL.logInfoApplication "sdl final"
  SDL.quit

initVideo :: GraphicsConfig -> IO ()
initVideo cfg = do
    SDL.logInfoApplication "video init"
    checkRet "sdl_subsys_video_init" =<< SDL.initSubSystem initFlagVideo
    runBracketM $ do
      driver <- case driver cfg of
        "" -> lift $ return nullPtr
        _ -> bracketM (newCString $ driver cfg) free
      return $ checkRet "video_init" =<< SDL.videoInit driver
    glSetAttrs $ glframebuffer 8 8 8 8 24 8
    glSetAttrs $ gldoublebuffer True
    glSetAttrs $ glaccelerated True
    glSetAttrs $ gldebug True
    glSetAttrs $ glmsaa 0

finalVideo :: IO ()
finalVideo = do
  SDL.logInfoApplication "video final"
  SDL.videoQuit
  SDL.quitSubSystem SDL.initFlagVideo

initWindow :: GraphicsConfig -> CString -> IO SDL.Window
initWindow cfg title = runBracketM $ do
  lift $ SDL.logInfoApplication "window init"
  let (width,height) = resolution cfg
      x = SDL.windowPosUndefined
      y = SDL.windowPosUndefined
      flags = glwindowflags (fullscreen cfg) (borderless cfg) False False False
  return $ checkError "create_window" $ createWindow title x y (fromIntegral width) (fromIntegral height) flags

finalWindow :: SDL.Window -> IO ()
finalWindow window = do
  logInfoApplication "window final"
  destroyWindow window

initGLContext :: GraphicsConfig -> SDL.Window -> IO SDL.GLContext
initGLContext cfg window = do
  logInfoApplication "gl ctx init"
  let swap_interval = case vsync cfg of
        VSyncNone -> 0
        VSyncNormal -> 1
        VSyncTear -> -1
  ctx <- checkError "create_gl_context" $ glCreateContext window
  checkRet "gl_set_swap_interval" =<< glSetSwapInterval swap_interval
  return ctx

finalGLContext :: SDL.GLContext -> IO ()
finalGLContext ctx = do
  logInfoApplication "gl ctx final"
  glDeleteContext ctx
