{-# LANGUAGE ScopedTypeVariables #-}
module Game.Centauri.Configuration
       ( GameConfig(..),
         loadGameConfig,
         loadDefaultGameConfig ) where
import qualified Data.Yaml.Config as Y
import Data.Text

data GameConfig = GameConfig { config_assetdir :: FilePath,
                               config_width :: Int,
                               config_height :: Int,
                               config_fullscreen :: Bool } deriving (Show)

_lookup s = Y.lookup (pack s)
_subconfig s = Y.subconfig (pack s)

loadGameConfig :: FilePath -> IO GameConfig
loadGameConfig path = do config <- Y.load path
                         assetConfig <- _subconfig "assets" config
                         assetdir <- _lookup "assetdir" assetConfig
                         graphicsConfig <- _subconfig "graphics" config
                         (width :: Int) <- _lookup "width" graphicsConfig
                         (height :: Int) <- _lookup "height" graphicsConfig
                         (fullscreen :: Bool) <- _lookup "fullscreen" graphicsConfig
                         return GameConfig { config_assetdir = assetdir,
                                             config_width = width,
                                             config_height = height,
                                             config_fullscreen = fullscreen}

loadDefaultGameConfig = loadGameConfig "/home/duncan/.centauri"
