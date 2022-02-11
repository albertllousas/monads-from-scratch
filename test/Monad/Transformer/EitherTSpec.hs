{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.EitherTSpec where

import Test.Hspec
import Monad.Transformer.EitherT
import Monad.Either
import Monad.Maybe
import Monad.IO
import Functor
import MonadTransformer
import Monad
import Data.Char
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (IO, Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Maybe, Monad)

type ReadLine = IO String

data InvalidCredential = InvalidCredential String deriving (Eq, Show)

fakeReadLine :: String -> ReadLine
fakeReadLine expectedInput = pure expectedInput

askPassword :: ReadLine -> String -> EitherT InvalidCredential IO String
askPassword readLine msg = do
                       _ <- lift $ putLine msg
                       password <- lift readLine
                       EitherT $ pure $ checkCredential password

checkCredential :: String -> Either InvalidCredential String
checkCredential password | length password < 8 = Left $ InvalidCredential "Too short"
                         | length password > 20 = Left $ InvalidCredential "Too long"
                         | not (isAlphaNumeric password) = Left $ InvalidCredential "Contains non alphanumeric chars"
                         | otherwise = Right password

isAlphaNumeric [] = False
isAlphaNumeric (x:[]) = isAlphaNum x
isAlphaNumeric (x:xs) = case isAlphaNum x of True -> isAlphaNumeric xs; False -> False

data PasswordStrength = Low | Medium | Strong deriving (Eq, Show)

checkPasswordStrength :: String -> PasswordStrength
checkPasswordStrength password | length password < 8 = Low
                               | length password > 15 = Strong
                               | otherwise = Medium

checkSamePassword :: String -> String -> Bool
checkSamePassword password password2 = password == password2

resetPassword :: String -> String -> EitherT InvalidCredential IO String
resetPassword a b = case checkSamePassword a b of
                                               True -> EitherT $ pure $ Right a -- some io to update the password
                                               False -> EitherT $ pure $ Left (InvalidCredential "Passwords don't match")

putLine :: String -> IO ()
putLine line = IO (\_ -> ((), ()))

spec :: Spec
spec = do

  describe "Running the either transformer" $ do

    it "should run a monad with error handling capabilities" $ do
      let io = (runEitherT (askPassword (fakeReadLine "Jane") "Please, insert your password:"))
      unsafePerformIO io `shouldBe` Left (InvalidCredential "Too short")

  describe "Using monad transformer instance of either transformer data type" $ do

    it "should lift (transform) a monad to an either transformer context" $ do
      let transformedIO = (lift $ fakeReadLine "hello") :: EitherT String IO String
      unsafePerformIO (runEitherT transformedIO) `shouldBe` Right "hello"

  describe "Using functor instance of either transformer data type" $ do

    it "should map over a transformed monad with error handling" $ do
      let io = (runEitherT (checkPasswordStrength <$> (askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:")))
      unsafePerformIO io `shouldBe` Right Medium

  describe "Using applicative instance of either transformer data type" $ do

    it "should lift a value into a monad enriched with error handling" $ do
      let maybeWithEither = (pure "hello") :: EitherT String Maybe String
      runEitherT maybeWithEither `shouldBe` Just (Right "hello")

    it "should apply a normal function (without dealing with monads) over transformed monads with error handling capabilities" $ do
      let password = askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:"
      let repeated = askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
      unsafePerformIO (runEitherT (checkSamePassword <$> password <*> repeated)) `shouldBe` Right True


  describe "Using monad instance of either transformer data type" $ do

    it "should combine computations that output transformed monads with error handling capabilities" $ do
      let ioWithEither = askPassword (fakeReadLine "JaneDoe1988") "Please, insert your password:" >>=
                                    (\a -> askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
                                          $> (\b -> checkSamePassword a b))
      unsafePerformIO (runEitherT ioWithEither) `shouldBe` Right True

    it "should simplify workflows that combine transformed monads with error handling using do-notation" $ do
      let reset = do
                    _ <- lift $ putLine "Password reset"
                    a <- askPassword (fakeReadLine "JaneDoe1988") "Please, insert your new password:"
                    b <- askPassword (fakeReadLine "JaneDoe1988") "Please, repeat the password:"
                    c <- resetPassword a b
                    return "Your password has been reset"
      unsafePerformIO (runEitherT reset) `shouldBe` Right "Your password has been reset"
