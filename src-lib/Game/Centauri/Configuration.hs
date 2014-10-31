module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig ) where
import Game.Centauri.Graphical.Configuration
import Game.Centauri.Assets.Configuration
import Game.Centauri.Graphical.UI.Configuration

data GameConfig = GameConfig { asset_config :: AssetConfig,
                               ui_config :: UIConfig,
                               graphics_config :: GraphicsConfig
                             } deriving (Show)

initGameConfig :: GameConfig
initGameConfig = GameConfig {
  asset_config = initAssetConfig,
  ui_config = initUIConfig,
  graphics_config = initGraphicsConfig }

loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = return initGameConfig
