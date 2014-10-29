module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig,
         loadDefaultGameConfig ) where
import qualified Data.ConfigFile as F
import Control.Applicative
import Foreign.C.Types

data GameConfig = GameConfig { assetdir :: FilePath,
                               width :: CInt,
                               height :: CInt,
                               fullscreen :: Bool,
                               borderless :: Bool,
                               vsync :: Bool,
                               vsync_late_tear :: Bool,
                               driver :: String
                             } deriving (Show)

loadGame file = do cp <- F.readstring F.emptyCP file
                   width <- read <$> F.get cp "graphics" "width"
                   height <- read <$> F.get cp "graphics" "height"
                   fullscreen <- read <$> F.get cp "graphics" "fullscreen"
                   borderless <- read <$> F.get cp "graphics" "borderless"
                   driver <- read <$> F.get cp "graphics" "driver"
                   vsync <- read <$> F.get cp "graphics" "vsync"
                   vsync_late_tear <- read <$> F.get cp "graphics" "vsync_late_tear"
                   assetdir <- read <$> F.get cp "main" "assetdir"
                   return GameConfig { assetdir = assetdir,
                                       width = width,
                                       height = height,
                                       fullscreen = fullscreen,
                                       borderless = borderless,
                                       vsync = vsync,
                                       vsync_late_tear = vsync_late_tear,
                                       driver = driver}
loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = do file <- readFile path
                         case loadGame file of
                           (Left err) -> fail (show err)
                           (Right cfg) -> return cfg

loadDefaultGameConfig = loadGameConfig "/home/duncan/.centauri"
