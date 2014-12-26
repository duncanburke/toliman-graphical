
module Game.Toliman.Graphical.Rendering.OpenGL
       (module Graphics.Rendering.OpenGL.GL,
        module Game.Toliman.Graphical.Rendering.OpenGL.Types)
       where

import Foreign.C.Types (CInt)

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.SDL

import Game.Toliman.Graphical.Rendering.OpenGL.Types


-- glSetAttrs :: (MonadGraphicalError m, MonadIO m) => GLAttrs -> m ()

-- glSetAttrs :: GLAttrList -> IO ()
-- -- glSetAttrs attrs = sequence_ [glSetAttribute attr val >>= sdlCheckRet desc | (desc, attr, val) <- attrs]
-- glSetAttrs = undefined
