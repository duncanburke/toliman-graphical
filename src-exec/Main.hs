
import Game.Centauri.Graphical
import Game.Centauri.Configuration

main :: IO ()
main = do cfg <- loadGameConfig ""
          graphicalMain cfg
