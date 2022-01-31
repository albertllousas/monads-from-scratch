module Monad.Transformer.StateT where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Monad, fmap)

-- | This transformer transforms a given monad adding it the state processing capabilities provided by the state monad, as a result a monad.
-- | Check the Test module to see examples of how to use it: 'StateTSpec'.

data StateT s m a = StateT (s -> m (s, a))

runStateT :: StateT s m a -> s -> m (s, a)
runStateT (StateT fn) state = fn state

instance Monad m => Functor (StateT s m) where
  fmap mapFn (StateT fn) = StateT $ \state -> fmap (\(state', a) -> (state', mapFn a)) (fn state)
