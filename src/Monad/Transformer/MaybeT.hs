module Monad.Transformer.MaybeT where

import Functor
import ApplicativeFunctor
import Monad.Maybe
import Monad
import MonadTransformer
import Prelude hiding (Functor, Monad, fmap, return, (>>=), (<$>), Maybe, Just, Nothing)

-- | This transformer transforms a given monad, adding optionality to the inner value, as a result, a new monad with combined functionalities.
-- | Check the Test module to see examples of how to use it: 'MaybeTSpec'.


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


--https://serokell.io/blog/whats-that-typeclass-foldable
--
--http://matija.me/2020/11/05/haskell-monad-transformers-intro/

-- https://making.pusher.com/unit-testing-io-in-haskell/ readerT for mocking