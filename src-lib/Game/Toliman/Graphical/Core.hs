
module Game.Toliman.Graphical.Core (
  graphicalMain) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Monad.Try (bracket_)

import Game.Toliman.Graphical.Types (
  GraphicalState, graphicalStateDefault,
  MonadGraphical )
import Game.Toliman.Graphical.State (
  initSDL, finalSDL,
  initSDLVideo,
  createWin,
  createGLCtx )
import Game.Toliman.Graphical.Rendering.Types (windowConfigDefault)
import Game.Toliman.Graphical.Internal.Errors (TolimanGraphicalError)
import Monad.Ref (RefT, runRefT)

graphicalMain :: IO ()
graphicalMain = do
  _ <- (flip runRefT) graphicalStateDefault $ runExceptT graphicalMain'
  return ()

graphicalMain' :: ExceptT TolimanGraphicalError (RefT GraphicalState IO) ()
graphicalMain' = do
  bracket_ initSDL finalSDL $ do
    initSDLVideo
    createWin windowConfigDefault
    createGLCtx
    gameLoop

gameLoop :: MonadGraphical ()
gameLoop = return ()
