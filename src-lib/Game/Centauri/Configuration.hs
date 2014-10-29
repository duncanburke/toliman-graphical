module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig,
         loadDefaultGameConfig ) where
import qualified Data.ConfigFile as F
import Control.Applicative
import Foreign.C.Types

import Game.Centauri.Graphical.Events

data GameConfig = GameConfig { assetdir :: FilePath,
                               width :: CInt,
                               height :: CInt,
                               fullscreen :: Bool,
                               borderless :: Bool,
                               vsync :: Bool,
                               vsync_late_tear :: Bool,
                               driver :: String,
                               evconfig :: EventConfig
                             } deriving (Show)

loadGame file = do cp <- F.readstring F.emptyCP file
                   width_ <- read <$> F.get cp "graphics" "width"
                   height_ <- read <$> F.get cp "graphics" "height"
                   fullscreen_ <- read <$> F.get cp "graphics" "fullscreen"
                   borderless_ <- read <$> F.get cp "graphics" "borderless"
                   driver_ <- read <$> F.get cp "graphics" "driver"
                   vsync_ <- read <$> F.get cp "graphics" "vsync"
                   vsync_late_tear_ <- read <$> F.get cp "graphics" "vsync_late_tear"
                   assetdir_ <- read <$> F.get cp "main" "assetdir"
                   evconfig_ <- return $ EventConfig
                   return GameConfig
                     {assetdir = assetdir_,
                      width = width_,
                      height = height_,
                      fullscreen = fullscreen_,
                      borderless = borderless_,
                      vsync = vsync_,
                      vsync_late_tear = vsync_late_tear_,
                      driver = driver_,
                      evconfig = evconfig_}

loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = do file <- readFile path
                         case loadGame file of
                           (Left err) -> fail (show err)
                           (Right cfg) -> return cfg

loadDefaultGameConfig = loadGameConfig "/home/duncan/.centauri"
