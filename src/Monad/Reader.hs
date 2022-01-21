module Monad.Reader where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Maybe, Monad)

-- | The Reader type represents a computation, which can read values from a shared environment, in other words, a way to inject dependencies to a function.
-- | Check the Test module to see examples of how to use it: 'ReaderSpec'.

data Reader env a = Reader (env -> a)

runReader :: env -> Reader env a -> a
runReader env (Reader fn) = fn env

instance Functor (Reader x) where
  fmap mapFn (Reader envToA) = Reader $ \env -> mapFn (envToA env)

instance ApplicativeFunctor (Reader x) where
   pure a = Reader $ \env -> a
   (<*>) (Reader envToFn) (Reader envToA) = Reader $ \env -> (envToFn env) (envToA env)

instance Monad (Reader x) where
  (>>=) (Reader envToA) aToReader = Reader $ \env -> runReader env (aToReader (envToA env))

ask :: Reader env env
ask = Reader $ \env -> env