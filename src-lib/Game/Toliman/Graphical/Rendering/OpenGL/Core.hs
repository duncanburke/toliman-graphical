
module Game.Toliman.Graphical.Rendering.OpenGL.Core (
 glSetAttrs,
 glGetErrors ) where

import Data.Functor ((<$>))

import Monad.Error (throwError)
import Control.Monad.Lift.IO (liftIO)
import Graphics.UI.SDL as SDL (
  glSetAttribute)

import Graphics.Rendering.OpenGL.Raw.Core31 (glGetError, gl_NO_ERROR)

import Game.Toliman.Graphical.Internal.Errors
import Game.Toliman.Graphical.SDL
import Game.Toliman.Graphical.Types

import Game.Toliman.Graphical.Rendering.OpenGL.Types

glSetAttrs :: GLAttrList -> MonadGraphical ()
glSetAttrs attrs = sequence_ [
  sdlCheckRet' desc $ glSetAttribute attr val | (desc, attr, val) <- attrs]

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
