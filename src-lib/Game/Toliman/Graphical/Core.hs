
module Game.Toliman.Graphical.Core (
  graphicalMain) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Lift.IO (liftIO)
import Monad.Try (bracket_)
import Monad.Ref (RefT, runRefT)

import System.Time.Monotonic (clockGetTime)
import Graphics.Rendering.OpenGL as GL

import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.Types
import Game.Toliman.Graphical.State

import qualified Game.Toliman.Graphical.Rendering as Rendering
import qualified Game.Toliman.Graphical.SDL as SDL
import qualified Game.Toliman.Graphical.UI as UI

graphicalMain :: IO ()
graphicalMain = do
  _ <- (flip runRefT) graphicalStateDefault $ runExceptT graphicalMain'
  return ()

graphicalMain' :: ExceptT TolimanGraphicalError (RefT GraphicalState IO) ()
graphicalMain' = do
  let sdl_config = SDL.sdlConfigDefault
  bracket_ (initSDL sdl_config) finalSDL $ do
    SDL.setLogPriorities $ SDL.logPrioritiesUniform SDL.SDL_LOG_PRIORITY_VERBOSE
    initSDLVideo sdl_config
    initSDLEvents sdl_config
    initClock
    createWin Rendering.windowConfigDefault
    createGLCtx Rendering.glConfigDefault
    initUIState
    gameLoop

gameLoop :: MonadGraphical ()
gameLoop = do
  (time .*=) =<< liftIO . clockGetTime =<< getJust "gameLoop: clock" (access clock)
  UI.syncUIState
  UI.runUI . UI.processEvents . (UI.translateSDLEvent <$>) =<< SDL.getEvents
  liftIO $ do
    GL.clearColor $= GL.Color4 0 0 1 0
    GL.clear [GL.ColorBuffer]
  Rendering.swapWindow
  gameLoop
