module Monad.Transformer.StateT where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Monad, fmap)

-- | The State Transformer allows to combine seamlessly the state monad with any other monad in our programs.
-- | Basically, a computation, which takes a state and returns a result along with a new state wrapped in another monad (a stateful computation wrapped in a monad).
-- | Check the Test module to see examples of how to use it: 'StateTSpec'.

data StateT s m a = StateT (s -> m (s, a))

runStateT :: StateT s m a -> s -> m (s, a)
runStateT (StateT fn) state = fn state

instance Monad m => Functor (StateT s m) where
  fmap mapFn (StateT fn) = StateT $ \state -> fmap (\(state', a) -> (state', mapFn a)) (fn state)

-- vending machine example