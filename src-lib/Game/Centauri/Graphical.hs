{- |
   module      : Game.Centauri.Graphical
   copyright   : (c) Duncan Burke
   license     : MPL
   maintaner   : Duncan Burke <duncankburke@gmail.cmo>
-}

module Game.Centauri.Graphical
       ( graphicalMain ) where

import Game.Centauri.Configuration

graphicalMain :: GameConfig -> IO ()
graphicalMain cfg = print cfg
