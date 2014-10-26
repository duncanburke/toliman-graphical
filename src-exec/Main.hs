
import Game.Centauri.Graphical
import Game.Centauri.Configuration

main = do cfg <- loadDefaultGameConfig
          graphicalMain cfg
