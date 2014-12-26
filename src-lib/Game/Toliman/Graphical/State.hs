
module Game.Toliman.Graphical.State where

import Data.Maybe (isNothing)
import Control.Applicative ((<$>))
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)

import Monad.Error (throwError)
import Control.Monad.Lift.IO (liftIO)
import Graphics.UI.SDL (createWindow,
                        destroyWindow,
                        glCreateContext,
                        glSetSwapInterval,
                        glDeleteContext)
import Graphics.Rendering.OpenGL.Raw.Core31 (glGetError, gl_NO_ERROR)

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.SDL
import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.Rendering
import Game.Toliman.Graphical.Rendering.OpenGL (toGLErrorFlag)
import Game.Toliman.Graphical.Rendering.Window (fromWindowFlags,
                                                fromWindowPos)

initSDLVideo :: MonadGraphical ()
initSDLVideo = undefined

finalSDLVideo :: MonadGraphical ()
finalSDLVideo = undefined

-- | The 'createWin' function creates a new window. A window may not already exist.
createWin :: WindowConfig -> MonadGraphical ()
createWin _win_config = do
  check "sdl video" $ access $ sdl.video
  check "Nothing window" $ isNothing <$> (access $ renderer.window)
  _win_title <- liftIO $ newCString $ _win_config ^. title
  let fl = fromWindowFlags $ _win_config ^. flags
      (x,y) = fromWindowPos $ _win_config ^. pos
      (w,h) = _win_config ^. resolution
  _win_handle <- sdlCheckNull' "CreateWindow" $ createWindow _win_title x y w h fl
  (renderer.window) .* Just Window {..}

-- | The 'destroyWin' function destroys the window if it exists, and any dependent state. This function is idempotent.
destroyWin :: MonadGraphical ()
destroyWin = do
  win' :: Maybe Window <- access $ renderer.window
  case win' of
   Nothing -> return ()
   Just win -> do
     liftIO $ destroyWindow $ win ^. handle
     liftIO $ free $ win ^. title
     (renderer.window) .* Nothing


initRenderer :: MonadGraphical ()
initRenderer = undefined

finalRenderer :: MonadGraphical ()
finalRenderer = undefined


-- | The 'createGLCtx' function creates an OpenGL context for the existing window.
-- This requires that 'window' has been initialised. An OpenGL context may not already exist.
createGLCtx :: MonadGraphical ()
createGLCtx = do
  check "Nothing glctx" $ isNothing <$> (access $ renderer.glctx)
  win <- getJust "window" $ accessPrism $ renderer.window._Just.handle
  ctx <- sdlCheckNull' "CreateContext" $ glCreateContext win
  (renderer.glctx) .* Just ctx
  let swap_interval = 0
  _ <- sdlCheckRet' "gl_set_swap_interval" $ glSetSwapInterval swap_interval
  return ()

-- | The 'destroyGLCtx' function destroys the OpenGL context if it exists, and any dependent state. This function is idempotent.
destroyGLCtx :: MonadGraphical ()
destroyGLCtx = do
  ctx' <- access $ renderer.glctx
  case ctx' of
   Nothing -> return ()
   Just ctx -> do
     ensure (isNothing <$> (access $ renderer.window)) destroyWin
     liftIO $ glDeleteContext ctx
     (renderer.glctx) .* Nothing

-- | The 'glGetErrors' function throws a GLError through 'MonadError' if any OpenGL errors have occurred.
glGetErrors :: MonadGraphical ()
glGetErrors = do
  l <- glGetErrors'
  case l of
   [] -> return ()
   _ -> throwError $ GLError l
  where glGetErrors' :: MonadGraphical [GLErrorFlag]
        glGetErrors' = do
          e <- liftIO glGetError
          if | e == gl_NO_ERROR -> return []
             | otherwise -> (toGLErrorFlag e :) <$> glGetErrors'
