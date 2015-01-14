
module Game.Toliman.Graphical.Core (
  graphicalMain) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Control.Monad.Lift.IO (liftIO)
import Monad.Try (bracket_)
import Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL as GL

import Monad.Ref (RefT, runRefT)
import Game.Toliman.Graphical.Types (
  GraphicalState, graphicalStateDefault,
  MonadGraphical)
import Game.Toliman.Graphical.State (
  initSDL, finalSDL,
  initSDLVideo,
  createWin,
  createGLCtx)
import Game.Toliman.Graphical.Rendering.Types (
  windowConfigDefault)
import Game.Toliman.Graphical.Rendering.Window (
  swapWindow)
import Game.Toliman.Graphical.Rendering.OpenGL.Types (
  glConfigDefault)
import Game.Toliman.Graphical.Internal.Errors (
  TolimanGraphicalError)
import Game.Toliman.Graphical.SDL (
  setLogPriorities,
  logPrioritiesUniform,
  pattern SDL_LOG_PRIORITY_VERBOSE)

graphicalMain :: IO ()
graphicalMain = do
  _ <- (flip runRefT) graphicalStateDefault $ runExceptT graphicalMain'
  return ()

graphicalMain' :: ExceptT TolimanGraphicalError (RefT GraphicalState IO) ()
graphicalMain' = do
  bracket_ initSDL finalSDL $ do
    setLogPriorities $ logPrioritiesUniform SDL_LOG_PRIORITY_VERBOSE
    initSDLVideo
    createWin windowConfigDefault
    createGLCtx glConfigDefault
    gameLoop

gameLoop :: MonadGraphical ()
gameLoop = do
  liftIO $ do
    GL.clearColor $= GL.Color4 0 0 1 0
    GL.clear [GL.ColorBuffer]
  swapWindow
  liftIO $ SDL.delay 1000
  gameLoop
