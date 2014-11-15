
module Game.Toliman.Graphical.Internal.Types where

import Control.Lens (makeLensesWith, underscoreFields)
import Language.Haskell.TH (Name, DecsQ)

makeUnderscoreFields :: Name -> DecsQ
makeUnderscoreFields = makeLensesWith underscoreFields
