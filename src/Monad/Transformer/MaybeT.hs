module Monad.Transformer.MaybeT where

import Functor
import ApplicativeFunctor
import Monad.Maybe
import Monad
import MonadTransformer
import Prelude hiding (Functor, Monad, fmap, return, (>>=), (<$>), Maybe, Just, Nothing)

data MaybeT m a = MaybeT (m (Maybe a))

runMaybeT (MaybeT m) = m

instance Monad m => Functor (MaybeT m) where
  fmap fn (MaybeT m) = MaybeT $ fmap (\maybe -> fmap fn maybe) m

instance Monad m => ApplicativeFunctor (MaybeT m) where
  pure a = MaybeT $ return (Just a)
  (<*>) (MaybeT mMaybeFn) (MaybeT mMaybeA) = MaybeT $ mMaybeFn >>=
      (\maybeFn -> case maybeFn of
                                 Nothing -> return Nothing
                                 (Just fn) -> mMaybeA >>=
                                                   (\maybeA -> case maybeA of
                                                                           Nothing -> return Nothing
                                                                           (Just a) -> return (Just $ fn a)))

instance Monad m => Monad (MaybeT m) where
  (>>=) (MaybeT mMaybeA) fn = MaybeT $ mMaybeA >>= \maybe -> case maybe of
                                                                         Nothing -> return Nothing
                                                                         (Just a) -> runMaybeT (fn a)

instance MonadTransformer MaybeT where
  lift m = MaybeT $ fmap (\a -> Just a) m

--https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
--
--read from db?
--
--
--https://serokell.io/blog/whats-that-typeclass-foldable
--
--https://www.parsonsmatt.org/2016/11/18/clean_alternatives_with_maybet.html
--
--http://matija.me/2020/11/05/haskell-monad-transformers-intro/

-- https://making.pusher.com/unit-testing-io-in-haskell/ readerT for mocking