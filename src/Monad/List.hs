module Monad.List where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, pure, Monad, List, fmap, (<*>), concat, (>>=))

-- | Check the Test module to see examples of how to use it: 'ListSpec'.

data List a =  Nil | Cons a (List a) deriving (Eq, Show)

concat :: List a -> List a -> List a
concat xs Nil = xs
concat Nil ys = ys
concat (Cons x xs) ys = Cons x (xs `concat` ys)

instance Functor List where
  fmap fn (Cons x xs) = Cons (fn x) (fmap fn xs)
  fmap _ Nil = Nil

instance ApplicativeFunctor List where
  pure a = Cons a Nil
  (<*>) (Cons fn fns) xs = (fmap fn xs) `concat` (fns <*> xs)
  (<*>) _ _ = Nil

instance Monad List where
  (>>=) (Cons x xs) fn = fn x `concat` ( xs >>= fn)
  (>>=) _ _ = Nil
