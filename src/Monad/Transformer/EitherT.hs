module Monad.Transformer.EitherT where

import Functor
import ApplicativeFunctor
import Monad
import MonadTransformer
import Monad.Either
import Prelude hiding (Either, Functor, Monad, fmap, return, (>>=), (<$>), Right, Left)

-- | This transformer transforms a given monad, adding to it error handling capabilities to the inner value, as a result, a new monad with combined functionalities.
-- | This transformer helps to reduce the boilerplate code generated when we combine monads with either.
-- | Check the Test module to see examples of how to use it: 'StateTSpec'.

data EitherT e m a = EitherT (m (Either e a))

runEitherT (EitherT m) = m

instance Monad m => Functor (EitherT e m) where
  fmap fn (EitherT mEitherA) = EitherT $ (\either -> fmap fn either) <$> mEitherA
--  fmap f = EitherT . fmap (fmap f) . runEitherT

instance Monad m => ApplicativeFunctor (EitherT e m) where
  pure a = EitherT $ return (Right a)
  (<*>) (EitherT mEitherFn) (EitherT mEitherA) = EitherT $ mEitherFn >>=
    (\eitherFn -> case eitherFn of
                                (Left fnE) -> return (Left fnE)
                                (Right fn) -> mEitherA >>=
                                  (\eitherA -> case eitherA of
                                                             (Left e) -> return (Left e)
                                                             (Right a) -> return (Right $ fn a)))

instance Monad m => Monad (EitherT e m) where
  (>>=) (EitherT mEitherA) fn = EitherT $ mEitherA >>= \either -> case either of (Left e) -> return (Left e); (Right a) -> runEitherT (fn a)

instance MonadTransformer (EitherT e) where
  lift m = EitherT $ fmap (\a -> Right a) m