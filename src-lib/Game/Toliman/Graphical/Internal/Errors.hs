
module Game.Toliman.Graphical.Internal.Errors
       (TolimanGraphicalError(..),
        GLErrorFlag(..),
        MonadGraphicalError,
        getJust,
        ensure,
        check,
        checkError) where

import Control.Exception
import Data.Typeable

import Monad.Error (MonadError, throwError)

import Game.Toliman.Graphical.Rendering.OpenGL.Types (GLErrorFlag(..))

data TolimanGraphicalError =
  SDLError String |
  GLError [GLErrorFlag] |
  BadState String
  deriving (Show, Typeable)

instance Exception TolimanGraphicalError

type MonadGraphicalError = MonadError TolimanGraphicalError

-- | The 'getJust' function extracts the element out of a 'Just' and
-- throws a 'BadState' error with the provided description if it is
-- 'Nothing'
getJust :: (MonadGraphicalError m) => String -> m (Maybe a) -> m a
getJust desc m = do
  x <- m
  case x of
   Just x' -> return x'
   Nothing -> throwError $ BadState desc

-- | The 'ensure' function checks the predicate in the first argument.
-- If this predicate fails, the second argument is run.
ensure :: (MonadGraphicalError m) => m Bool -> m () -> m ()
ensure m n = do
  p <- m
  if | not p -> n
     | otherwise -> return ()

-- | The 'check' function throws a 'BadState' error with the provided
-- description if the predicate fails.
check :: (MonadGraphicalError m) => String -> m Bool -> m ()
check desc m = do
  p <- m
  if | not p -> throwError $ BadState desc
     | otherwise -> return ()

checkError :: (MonadGraphicalError m) => Either TolimanGraphicalError a -> m a
checkError (Left e) = throwError e
checkError (Right a) = return a
