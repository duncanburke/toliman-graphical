module Game.Centauri.Graphical.UI.Configuration
       (UIConfig(..),
        initUIConfig) where

data UIConfig = UIConfig {
  mouse_relative :: Bool,
  mouse_capture :: Bool,  -- todo
  mouse_hide :: Bool, -- todo
  mouse_scale :: Float } deriving (Show)

initUIConfig :: UIConfig
initUIConfig = UIConfig {
  mouse_relative = True,
  mouse_capture = True,
  mouse_hide = True,
  mouse_scale = 1.0 }
