
module Game.Toliman.Graphical.Rendering.OpenGL
       (module Graphics.Rendering.OpenGL.GL,
        module Game.Toliman.Graphical.Rendering.OpenGL.Types)
       where


import Graphics.Rendering.OpenGL.GL

import Game.Toliman.Graphical.Rendering.OpenGL.Types


-- glSetAttrs :: (MonadGraphicalError m, MonadIO m) => GLAttrs -> m ()

-- glSetAttrs :: GLAttrList -> IO ()
-- -- glSetAttrs attrs = sequence_ [glSetAttribute attr val >>= sdlCheckRet desc | (desc, attr, val) <- attrs]
-- glSetAttrs = undefined
