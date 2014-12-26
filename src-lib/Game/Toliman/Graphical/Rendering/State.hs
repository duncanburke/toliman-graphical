
module Game.Toliman.Graphical.Rendering.State where

import Data.Maybe (isNothing)
import Control.Applicative ((<$>))

import Monad.Error (throwError)
import Control.Monad.Lift.IO (liftIO)
import Graphics.UI.SDL (glCreateContext,
                        glSetSwapInterval,
                        glDeleteContext)
import Graphics.Rendering.OpenGL.Raw.Core31 (glGetError, gl_NO_ERROR)

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.SDL
import Game.Toliman.Graphical.Rendering.Types
import Game.Toliman.Graphical.Rendering.OpenGL

initRenderer :: MonadRenderer ()
initRenderer = undefined

finalRenderer :: MonadRenderer ()
finalRenderer = undefined

initSDLVideo :: MonadRenderer ()
initSDLVideo = undefined

finalSDLVideo :: MonadRenderer ()
finalSDLVideo = undefined

createWin :: WindowConfig -> MonadRenderer ()
createWin = undefined

destroyWin :: MonadRenderer ()
destroyWin = undefined

--  win <- getJust "window" $ access $ window._Just.handle

-- | The 'createGLCtx' function creates an OpenGL context for the
-- existing window. This requires that 'window' has been initialised.
-- An OpenGL context may not already exist.
createGLCtx :: MonadRenderer ()
createGLCtx = do
  check "Nothing glctx" $ isNothing <$> access glctx
  win <- getJust "window" $ accessPrism $ window._Just.handle
  ctx <- liftIO $ glCreateContext win
  glctx .* Just ctx
  let swap_interval = 0
  _ <- sdlCheckRet "gl_set_swap_interval" =<< (liftIO $ glSetSwapInterval swap_interval)
  return ()

-- | The 'destroyGLCtx' function destroys the OpenGL context and any dependent
-- state, if it exists. This function is idempotent.
destroyGLCtx :: MonadRenderer ()
destroyGLCtx = do
  ctx' <- access glctx
  case ctx' of
   Nothing -> return ()
   Just ctx -> do
     ensure (isNothing <$> access window) destroyWin
     liftIO $ glDeleteContext ctx

-- |Throws a GLError if any OpenGL errors have occurred
-- This clears any errors.
glGetErrors :: MonadRenderer ()
glGetErrors = do
  l <- glGetErrors'
  case l of
   [] -> return ()
   _ -> throwError $ GLError l
  where glGetErrors' :: MonadRenderer [GLErrorFlag]
        glGetErrors' = do
          e <- liftIO glGetError
          if | e == gl_NO_ERROR -> return []
             | otherwise -> (toGLErrorFlag e :) <$> glGetErrors'
