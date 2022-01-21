module Functor where

import Prelude hiding (Functor, fmap, (<$>))

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap

-- flipped fmap
($>) :: Functor f => f a -> (a -> b) -> f b
($>) f fn = fmap fn f
