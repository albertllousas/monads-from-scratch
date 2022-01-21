module Monad.IO where

import Functor
import ApplicativeFunctor
import Monad
import Prelude hiding (IO, Functor, pure, Monad)

type OutsideWorld = ()

-- | A value of type IO a is a computation which, when performed, does some I/O before returning a value of type a.
-- | Check the Test module to see examples of how to use it: 'IOSpec'.

newtype IO a = IO (OutsideWorld -> (a, OutsideWorld))

unsafePerformIO :: IO a -> a
unsafePerformIO (IO fn) = case fn () of (a, _) -> a

instance Functor IO where
  fmap fn (IO action) = IO $ \outsideWorld -> case action () of (a, outsideWorld') -> (fn a, outsideWorld')

instance ApplicativeFunctor IO where
  pure a = IO $ \outsideWorld -> (a, ())
  (<*>) (IO actionFn) (IO actionA) = IO $ \outsideWorld -> (fn a, ())
    where (a, _) = actionA ()
          (fn, _) = actionFn ()

instance Monad IO where
  (>>=) io fn = IO $ \outsideWorld -> case executeIO io of (a, outsideWorld') -> executeIO (fn a)
    where executeIO :: IO a -> (a, OutsideWorld)
          executeIO (IO action) = action ()
