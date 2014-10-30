module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig ) where
import Game.Centauri.Graphical.Configuration
import Game.Centauri.Assets.Configuration
import Game.Centauri.Graphical.UI.Configuration

data GameConfig = GameConfig { ascfg :: AssetConfig,
                               uicfg :: UIConfig,
                               grcfg :: GraphicsConfig
                             } deriving (Show)

initGameConfig :: GameConfig
initGameConfig = GameConfig {
  ascfg = initAssetConfig,
  uicfg = initUIConfig,
  grcfg = initGraphicsConfig }

loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = return initGameConfig
