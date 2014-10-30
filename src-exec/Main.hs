
import Game.Centauri.Graphical
import Game.Centauri.Configuration

main = do cfg <- loadGameConfig ""
          graphicalMain cfg
