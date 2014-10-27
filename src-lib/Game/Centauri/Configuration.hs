module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig,
         loadDefaultGameConfig ) where
import qualified Data.ConfigFile as F
import Control.Applicative
import Foreign.C.Types

data GameConfig = GameConfig { config_assetdir :: FilePath,
                               config_width :: CInt,
                               config_height :: CInt,
                               config_fullscreen :: Bool,
                               config_borderless :: Bool,
                               config_vsync :: Bool,
                               config_vsync_late_tear :: Bool,
                               config_driver :: String
                             } deriving (Show)

loadGameConfig_ file = do cp <- F.readstring F.emptyCP file
                          width <- read <$> F.get cp "graphics" "width"
                          height <- read <$> F.get cp "graphics" "height"
                          fullscreen <- read <$> F.get cp "graphics" "fullscreen"
                          borderless <- read <$> F.get cp "graphics" "borderless"
                          driver <- read <$> F.get cp "graphics" "driver"
                          vsync <- read <$> F.get cp "graphics" "vsync"
                          vsync_late_tear <- read <$> F.get cp "graphics" "vsync_late_tear"
                          assetdir <- read <$> F.get cp "main" "assetdir"
                          return GameConfig { config_assetdir = assetdir,
                                              config_width = width,
                                              config_height = height,
                                              config_fullscreen = fullscreen,
                                              config_borderless = borderless,
                                              config_vsync = vsync,
                                              config_vsync_late_tear = vsync_late_tear,
                                              config_driver = driver}
loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = do file <- readFile path
                         case loadGameConfig_ file of
                           (Left err) -> fail (show err)
                           (Right cfg) -> return cfg

loadDefaultGameConfig = loadGameConfig "/home/duncan/.centauri"
