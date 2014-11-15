
module Game.Toliman.Graphical.Rendering.State where

import Monad.State (MonadState)
import Control.Monad.Lift.IO (MonadIO, liftIO)

import Foreign.Ptr (nullPtr)

import Graphics.UI.SDL

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.SDL
import Game.Toliman.Graphical.Rendering.Types
import Game.Toliman.Graphical.Rendering.OpenGL

type RendererM = (MonadState RendererState m, MonadIO m) => m () 

initRenderer :: RendererM
initRenderer = undefined

finalRenderer :: RendererM
finalRenderer = undefined

initSDLVideo :: RendererM
initSDLVideo = undefined

finalSDLVideo :: RendererM
finalSDLVideo = undefined

createWin :: RendererM
createWin = undefined

createGLCtx :: RendererM
createGLCtx = do
  let swap_interval = 0
  Just win <- use window
  ctx <- liftIO $ glCreateContext win
  _ <- checkRet "gl_set_swap_interval" =<< (liftIO $ glSetSwapInterval swap_interval)
  return ()
