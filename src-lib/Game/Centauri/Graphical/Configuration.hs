module Game.Centauri.Graphical.Configuration
       ( GraphicsConfig(..),
         GraphicsVSyncMode(..),
         initGraphicsConfig
       ) where

import Foreign.C.Types
import Graphics.UI.SDL

data GraphicsVSyncMode = VSyncNone | VSyncNormal | VSyncTear deriving (Read, Show)

data GraphicsConfig = GraphicsConfig {
  resolution :: (Int,Int),
  fullscreen :: Bool,
  borderless :: Bool,
  vsync :: GraphicsVSyncMode,
  driver :: String,
  sdl_log_pri :: [(CInt,LogPriority)]
  } deriving (Read, Show)

initGraphicsConfig :: GraphicsConfig
initGraphicsConfig = GraphicsConfig {
  resolution = (1024,768),
  fullscreen = False,
  borderless = False,
  vsync = VSyncNone,
  driver = "",
  sdl_log_pri = []}
