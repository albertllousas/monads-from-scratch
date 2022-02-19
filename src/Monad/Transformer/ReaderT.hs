module Monad.Transformer.ReaderT where


import Functor
import ApplicativeFunctor
import Monad.Maybe
import Monad
import MonadTransformer
import Prelude hiding (Functor, Monad, fmap, return, (>>=), (<$>), Maybe, Just, Nothing)

-- | This transformer transforms a given monad, injecting a dependency to compute it, as a result, a new monad with combined functionalities.
-- | Check the Test module to see examples of how to use it: 'ReaderTSpec'.

data ReaderT env m a = ReaderT (env -> m a)

runReaderT (ReaderT fn) env = fn env

instance Monad m => Functor (ReaderT env m) where
  fmap fn (ReaderT envToM) = ReaderT $ \env -> fmap (\a-> fn a) (envToM env)

instance Monad m => ApplicativeFunctor (ReaderT env m) where
  pure a = ReaderT $ \_ -> return a
  (<*>) (ReaderT envToMFn) (ReaderT envToMA) = ReaderT $ \env -> (envToMFn env) >>= (\fn -> (envToMA env) $> \a -> fn a)

instance Monad m => Monad (ReaderT env m) where
  (>>=) (ReaderT envToMA) fn = ReaderT $ \env -> (envToMA env) >>= (\ma -> (envToMA env) >>= \a -> runReaderT (fn a) env)

instance MonadTransformer (ReaderT env) where
  lift m = ReaderT $ \env -> m