module Monad where

import ApplicativeFunctor
import Prelude hiding (pure)

class ApplicativeFunctor m => Monad m where
  (>>=) :: m a -> (a -> m b ) -> m b
  return :: a -> m a
  return = pure