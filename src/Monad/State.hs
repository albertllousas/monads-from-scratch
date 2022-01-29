module Monad.State where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (Functor, Monad)

-- | The State type represents a computation, which takes a state and returns a result along with a new state (a pure stateful computation).
-- | Check the Test module to see examples of how to use it: 'ReaderSpec'.

data State s a = State(s -> (s, a))

runState :: State s a -> s -> (s, a)
runState (State fn) state = fn state

instance Functor (State s) where
  fmap mapFn (State fn) = State $ \state -> case fn state of (newState, a) -> (newState, mapFn a)

instance ApplicativeFunctor (State s) where
  pure a = State $ \state -> (state, a)
  (<*>) (State fnFn) (State fnA) = State $ \state -> let (state', fn) = fnFn state
                                                         (state'', a) = fnA state'
                                                     in (state'', fn a)

instance Monad (State s) where
  (>>=) (State fnA) fn = State $ \state -> case fnA state of (newState, a) -> runState (fn a) newState

get = State $ \state -> (state, state)

put newState = State $ \state -> (newState, ())

