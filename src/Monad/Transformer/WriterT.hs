module Monad.Transformer.WriterT where

import Functor
import ApplicativeFunctor
import Monad
import MonadTransformer
import Prelude hiding (Functor, Monad, fmap, return, (>>=), (<$>), pure)

-- | This transformer transforms a given monad, accumulating a log of data in addition to the computed values, as a result, a new monad with combined functionalities.
-- | Check the Test module to see examples of how to use it: 'WriterTSpec'.

data WriterT log m a = WriterT { runWriterT :: m (log, a) }

instance Monad m => Functor (WriterT log m) where
  fmap fn (WriterT m) = WriterT $ fmap (\(log, a) -> (log, fn a)) m

instance (Monad m, Monoid log) => ApplicativeFunctor (WriterT log m) where
  pure a = WriterT $ pure (mempty, a)
  (<*>) (WriterT mLogFn) (WriterT mLogA) = WriterT $ mLogFn >>= (\(logFn, fn) -> mLogA $> \(logA, a) -> (logFn <> logA, fn a))

instance (Monad m, Monoid log) => Monad (WriterT log m) where
  (>>=) (WriterT m) fn = WriterT $ m >>= (\(log, a) -> (runWriterT (fn a)) $> \(newLog, b) -> (log <> newLog, b))