module Monad.Reader where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Maybe, Monad)

-- | The Reader type represents a computation, which can read values from a shared environment, in other words, a way to inject dependencies to a function.
-- | Check the Test module to see examples of how to use it: 'ReaderSpec'.

data Reader env a = Reader (env -> a)
--or just : newtype Reader env a = Reader { runReader :: env -> a }

runReader :: Reader env a -> env -> a
runReader (Reader fn) env = fn env

instance Functor (Reader x) where
  fmap mapFn (Reader envToA) = Reader $ \env -> mapFn (envToA env)

instance ApplicativeFunctor (Reader x) where
   pure a = Reader $ \env -> a
   (<*>) (Reader envToFn) (Reader envToA) = Reader $ \env -> (envToFn env) (envToA env)

instance Monad (Reader x) where
  (>>=) (Reader envToA) aToReader = Reader $ \env -> runReader (aToReader (envToA env)) env

ask :: Reader env env
ask = Reader $ \env -> env