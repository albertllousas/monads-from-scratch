module Monad.Writer where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Maybe, Monad)

-- | The Writer monad represents computations which produce an accumulated log of data in addition to the computed values. (log an execution of functions)
-- | Check the Test module to see examples of how to use it: 'WriterSpec'.

data Writer log a = Writer (log, a)

runWriter :: Writer log a -> (log, a)
runWriter (Writer (log, a)) = (log, a)

instance Functor (Writer l) where
  fmap fn (Writer (log, a)) = Writer (log, fn a)

instance Monoid log => ApplicativeFunctor (Writer log) where
  pure a = Writer (mempty, a)
  (<*>) (Writer (logFn, fn)) (Writer (logA, a)) = Writer (logFn <> logA, fn a)

instance Monoid log => Monad (Writer log) where
  (>>=) (Writer (log, a)) fn = Writer(log <> newLog, b)
    where Writer (newLog, b) = fn a

tell :: log -> Writer log ()
tell l = Writer (l, ())
