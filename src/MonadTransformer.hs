module MonadTransformer where

import Monad
import Prelude hiding (Monad)

class MonadTransformer mt where
  lift :: Monad m => m a -> mt m a