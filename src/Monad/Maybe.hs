module Monad.Maybe where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Maybe, Just, Nothing, Monad)

-- | The Maybe type encapsulates an optional value, either contains a value of type a, or it is empty.
-- | Check the Test module to see examples of how to use it: 'MaybeSpec'.

data Maybe a = Nothing | Just a deriving (Eq, Show)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap fn (Just a) = Just $ fn a

instance ApplicativeFunctor Maybe where
  pure a = Just a
  (<*>) (Just fn) (Just a) = Just $ fn a
  (<*>) _ _ = Nothing

instance Monad Maybe where
  (>>=) (Just a) fn = fn a
  (>>=) Nothing fn = Nothing

