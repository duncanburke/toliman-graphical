
module Game.Toliman.Graphical.State (
  initSDL, finalSDL,
  initSDLVideo, finalSDLVideo,
  createWin, destroyWin,
  createGLCtx, destroyGLCtx
  ) where

import Data.Maybe (isNothing)
import Control.Applicative ((<$>))
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)

import Control.Monad.Lift.IO (liftIO)
import Monad.Error (catchError, throwError)
import Monad.Mask (mask_)
import Graphics.UI.SDL as SDL (
  init, quit,
  initSubSystem, quitSubSystem,
  pattern SDL_INIT_VIDEO,
  createWindow, destroyWindow,
  glCreateContext, glDeleteContext,
  glSetSwapInterval)

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.SDL
import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.Rendering
import Game.Toliman.Graphical.Rendering.Window (fromWindowFlags,
                                                fromWindowPos)
import Game.Toliman.Graphical.Rendering.OpenGL (toGLAttrList,
                                                glSetAttrs)

initSDL :: MonadGraphical ()
initSDL = mask_ $ do
  check "not init_sdl" $ not <$> (access $ sdl.init_sdl)
  _ <- sdlCheckRet' "init" $ SDL.init 0
  (sdl.init_sdl) .* True

finalSDL :: MonadGraphical ()
finalSDL = mask_ $ do
  sdl_init' <- access $ sdl.init_sdl
  case sdl_init' of
   False -> return ()
   True -> do
     ensure (not <$> (access $ sdl.init_video)) finalSDLVideo
     liftIO SDL.quit
     (sdl.init_sdl) .* False

initSDLVideo :: MonadGraphical ()
initSDLVideo = mask_ $ do
  check "not init_video" $ not <$> (access $ sdl.init_video)
  _ <- sdlCheckRet' "initVideo" $ SDL.initSubSystem SDL_INIT_VIDEO
  (sdl.init_video) .* True

finalSDLVideo :: MonadGraphical ()
finalSDLVideo = mask_ $ do
  init_video' <- access $ sdl.init_video
  case init_video' of
   False -> return ()
   True -> do
     ensure (isNothing <$> (access $ renderer.window)) destroyWin
     liftIO $ SDL.quitSubSystem SDL_INIT_VIDEO
     (sdl.init_video) .* False

-- | The 'createWin' function creates a new window. A window must not already exist.
createWin :: WindowConfig -> MonadGraphical ()
createWin _win_config = mask_ $ do
  check "sdl video" $ access $ sdl.init_video
  check "isNothing window" $ isNothing <$> (access $ renderer.window)
  glSetAttrs . toGLAttrList $ _win_config ^. gl_attrs
  _win_title <- liftIO $ newCString $ _win_config ^. title
  flip catchError (\e -> (liftIO $ free _win_title) >> throwError e) $ do
    let flags' = fromWindowFlags $ _win_config ^. flags
        (x,y) = fromWindowPos $ _win_config ^. pos
        (w,h) = _win_config ^. resolution
    _win_handle <- sdlCheckNull' "CreateWindow" $ createWindow _win_title x y w h flags'
    (renderer.window) .* Just Window {..}

-- | The 'destroyWin' function destroys the window if it exists, and any dependent state. This function is idempotent.
destroyWin :: MonadGraphical ()
destroyWin = mask_ $ do
  win' :: Maybe Window <- access $ renderer.window
  case win' of
   Nothing -> return ()
   Just win -> do
     liftIO $ destroyWindow $ win ^. handle
     liftIO $ free $ win ^. title
     (renderer.window) .* Nothing

-- | The 'createGLCtx' function creates an OpenGL context for the existing window.
-- This requires that 'window' has been initialised. An OpenGL context must not already exist.
createGLCtx :: MonadGraphical ()
createGLCtx = mask_ $ do
  check "isNothing glctx" $ isNothing <$> (access $ renderer.glctx)
  win <- getJust "window" $ accessPrism $ renderer.window._Just.handle
  ctx <- sdlCheckNull' "CreateContext" $ glCreateContext win
  (renderer.glctx) .* Just ctx
  let swap_interval = 0
  _ <- sdlCheckRet' "gl_set_swap_interval" $ glSetSwapInterval swap_interval
  return ()

-- | The 'destroyGLCtx' function destroys the OpenGL context if it exists, and any dependent state. This function is idempotent.
destroyGLCtx :: MonadGraphical ()
destroyGLCtx = mask_ $ do
  ctx' <- access $ renderer.glctx
  case ctx' of
   Nothing -> return ()
   Just ctx -> do
     ensure (isNothing <$> (access $ renderer.window)) destroyWin
     liftIO $ glDeleteContext ctx
     (renderer.glctx) .* Nothing

initRenderer :: MonadGraphical ()
initRenderer = undefined

finalRenderer :: MonadGraphical ()
finalRenderer = undefined
