module Monad.Either where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Either, Right, Left, Monad)

-- | The Either type represents values with two possibilities: a value of type Either a b is either Left a or Right b.
-- | By convention, the Left constructor is used to hold an error value and the Right constructor is used to hold a correct value.
-- | Check the Test module to see examples of how to use it: 'EitherSpec'.

data Either a b = Left a | Right b deriving (Eq, Show)

instance Functor (Either x) where
  fmap _ (Left a) = Left a
  fmap fn (Right b) = Right (fn b)

instance ApplicativeFunctor (Either x) where
  pure a = Right a
  (<*>) (Right fn) (Right b) = Right $ fn b
  (<*>) (Left a) _ = Left a
  (<*>) _ (Left a) = Left a

instance Monad (Either x) where
  (>>=) (Right b) fn = fn b
  (>>=) (Left a) _ = Left a
