module ApplicativeFunctor where

import Prelude hiding (Functor)
import Functor

class Functor f => ApplicativeFunctor f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b