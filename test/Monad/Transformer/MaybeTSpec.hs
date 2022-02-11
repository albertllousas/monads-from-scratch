{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.MaybeTSpec where

import Test.Hspec
import Monad.Transformer.MaybeT
import Monad.Maybe
import Monad.IO
import Functor
import MonadTransformer
import Monad
import ApplicativeFunctor
import Prelude hiding (IO, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Maybe, Monad)

type UserId = String

data User = User { id :: String, name :: String }

findUserFromDB :: UserId -> MaybeT IO User


spec :: Spec
spec = do

  describe "Running the either transformer" $ do

    it "should run a monad with error handling capabilities" $ do
      let io = (runEitherT (askPassword (fakeReadLine "Jane") "Please, insert your password:"))
      unsafePerformIO io `shouldBe` Left (InvalidCredential "Too short")

--  describe "Using monad transformer instance of either transformer data type" $ do
--
--    it "should lift (transform) a monad to an either transformer context" $ do
--      let transformedIO = (lift $ fakeReadLine "hello") :: EitherT String IO String
--      unsafePerformIO (runEitherT transformedIO) `shouldBe` Right "hello"
--
--  describe "Using functor instance of either transformer data type" $ do
--
--    it "should map over a transformed monad with error handling" $ do
--      let io = (runEitherT (checkPasswordStrength <$> (askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:")))
--      unsafePerformIO io `shouldBe` Right Medium
--
--  describe "Using applicative instance of either transformer data type" $ do
--
--    it "should lift a value into a monad enriched with error handling" $ do
--      let maybeWithEither = (pure "hello") :: EitherT String Maybe String
--      runEitherT maybeWithEither `shouldBe` Just (Right "hello")
--
--    it "should apply a normal function (without dealing with monads) over transformed monads with error handling capabilities" $ do
--      let password = askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:"
--      let repeated = askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
--      unsafePerformIO (runEitherT (checkSamePassword <$> password <*> repeated)) `shouldBe` Right True
--
--
--  describe "Using monad instance of either transformer data type" $ do
--
--    it "should combine computations that output transformed monads with error handling capabilities" $ do
--      let ioWithEither = askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:" >>=
--                                    (\a -> askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
--                                          $> (\b -> checkSamePassword a b))
--      unsafePerformIO (runEitherT ioWithEither) `shouldBe` Right True
--
--
--    it "should simplify workflows that combine transformed monads with error handling using do-notation" $ do
--      let reset = do
--                    _ <- lift $ putLine "Password reset"
--                    a <- askPassword (fakeReadLine "JaneDoe1988") "Please, insert your new password:"
--                    b <- askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
--                    c <- resetPassword a b
--                    return "Your password has been reset"
--      unsafePerformIO (runEitherT reset) `shouldBe` Right "Your password has been reset"

