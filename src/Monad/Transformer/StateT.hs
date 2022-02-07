module Monad.Transformer.StateT where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Monad, fmap, return, (>>=))

-- | This transformer transforms a given monad, adding it to it the state processing capabilities provided by the state monad, as a result, a new monad with combined functionalities.
-- | Check the Test module to see examples of how to use it: 'StateTSpec'.

data StateT s m a = StateT (s -> m (s, a))

runStateT :: StateT s m a -> s -> m (s, a)
runStateT (StateT fn) state = fn state

instance Monad m => Functor (StateT s m) where
  fmap mapFn (StateT fn) = StateT $ \state -> fmap (\(state', a) -> (state', mapFn a)) (fn state)

instance Monad m => ApplicativeFunctor (StateT s m) where
  pure a =  StateT $ \state -> return (state, a)
  (<*>) (StateT fnMonadFn) (StateT fnMonadA) = StateT $ \s -> fnMonadFn s >>= \(s', fn) -> fnMonadA s' >>= \(s'', a) -> return (s'', fn a)

instance Monad m => Monad (StateT s m) where
  (>>=) (StateT fnMonadA) fn = StateT $ \state -> fnMonadA state >>= \(state', a) -> runStateT (fn a) state'
