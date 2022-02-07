{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.StateTSpec where

import Test.Hspec
import Monad.Transformer.StateT
import Monad.Either
import Functor
import Monad
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum)

type ConsecutiveFailAttempts = Int

type Limit  = Int

type Password = String

data LoginAttempt = ValidPassword | InvalidPassword deriving (Eq, Show)

data AccountLocked = AccountLocked deriving (Eq, Show)

type EitherWithState e s a = StateT s (Either e) a

checkLoginAttempt :: Limit -> Password -> Password -> EitherWithState AccountLocked ConsecutiveFailAttempts LoginAttempt
checkLoginAttempt l p1 p2 = StateT $ \failAttempts -> case () of
                                                              _ | (failAttempts == l) -> Left AccountLocked
                                                                | p1 /= p2 -> Right (failAttempts + 1, InvalidPassword)
                                                                | otherwise -> Right (0, ValidPassword)

spec :: Spec
spec = do

  describe "Running the state transformer" $ do

    it "should run a monad with state capabilities" $ do
      runStateT (checkLoginAttempt 5 "pass" "pass") 0 `shouldBe` Right (0, ValidPassword)
      runStateT (checkLoginAttempt 5 "pass" "pass") 5 `shouldBe` Left AccountLocked

  describe "Using functor instance of state transformer data type" $ do

    it "should map over a transformed monad with state" $ do
      let message :: LoginAttempt -> String
          message ValidPassword = "Hi there! Welcome again."
          message InvalidPassword = "Invalid credentials, please, try again."
      runStateT (message <$> (checkLoginAttempt 5 "pass" "pass")) 0 `shouldBe` Right (0, "Hi there! Welcome again.")
      runStateT (message <$> (checkLoginAttempt 5 "pass" "invalid")) 0 `shouldBe` Right (1, "Invalid credentials, please, try again.")

  describe "Using applicative instance of state transformer data type" $ do

    it "should lift a value into a monad with state capabilities" $ do
      let eitherWithState = (pure "hello") :: StateT () (Either String) String
      runStateT eitherWithState () `shouldBe` Right ((), "hello")

    it "should apply a normal function (without dealing with monads) over transformed monads with state capabilities" $ do
      let first = checkLoginAttempt 5 "pass" "pass"
      let second = checkLoginAttempt 5 "pass" "invalid"
      let combineAttempts :: LoginAttempt -> LoginAttempt -> [LoginAttempt]
          combineAttempts la1 la2 = [la1, la2]
      runStateT (pure combineAttempts <*> first <*> second) 0 `shouldBe` Right (1, [ValidPassword, InvalidPassword])

  describe "Using monad instance of state transformer data type" $ do

    it "should combine computations that output transformed monads with state capabilities" $ do
      let eitherWithState = checkLoginAttempt 5 "pass" "invalid" >>= \_ -> checkLoginAttempt 5 "pass" "invalid"
      runStateT eitherWithState 0 `shouldBe` Right (2, InvalidPassword)

    it "should simplify workflows that combine transformed monads with state using do-notation" $ do
      let eitherWithState = do
                           x <- checkLoginAttempt 5 "pass" "invalid"
                           y <- checkLoginAttempt 5 "pass" "invalid"
                           z <- checkLoginAttempt 5 "pass" "pass"
                           g <- checkLoginAttempt 5 "pass" "invalid"
                           return [x, y, z, g]
      runStateT eitherWithState 0 `shouldBe` Right (1,[InvalidPassword, InvalidPassword, ValidPassword, InvalidPassword])
