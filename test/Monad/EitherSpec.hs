{-# LANGUAGE RebindableSyntax #-}
module Monad.EitherSpec where

import Test.Hspec
import Monad.Either
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=))

data Account =  Account { balance :: Float, owner :: String } deriving (Eq, Show)

spec :: Spec
spec = do

  describe "Using functor instance of either data type" $ do

    it "should apply a function on an either that holds a right value" $ do
      let amount = (Right 100.0) :: Either String Float
      fmap (+100) amount `shouldBe` Right 200.0

    it "should not apply a function on an either that holds an error (left)" $ do
      let amount = Left "negative-amount"
      fmap (+100) amount `shouldBe` Left "negative-amount"

  describe "Using applicative instance of either data type" $ do

    it "should lift a right value into an either" $ do
      let amount = (pure 100.0) :: Either String Float
      amount `shouldBe` Right 100.0

    it "should allow to apply a 'normal' function (taking non-either args) to a couple of right values" $ do
       let amount = (Right 100.0) :: Either String Float
       let owner = (Right "Jane Doe") :: Either String String
       let invalidAmount = (Left "negative-amount") :: Either String Float
       let fraudster = (Left "fraud") :: Either String String
       pure Account <*> amount <*> owner `shouldBe` Right (Account {balance = 100.0, owner = "Jane Doe"}) -- type constructor is just another function
       pure Account <*> amount <*> fraudster `shouldBe` Left "fraud"
       pure Account <*> invalidAmount <*> owner `shouldBe` Left "negative-amount"

  describe "Using monad instance of either data type" $ do

    let createAccount :: String -> Account
        createAccount owner = Account { balance = 0.0, owner = owner }

    let deposit :: Float -> Account -> Either String Account
        deposit amount account | amount < 0 = Left "negative-amount"
                               | otherwise = Right $ account { balance = (balance account) + amount }

    let withdraw :: Float -> Account -> Either String Account
        withdraw amount account@(Account balance _) | amount < 0 = Left "negative-amount"
                                                    | (balance - amount) < 0 = Left "not-enough-balance"
                                                    | otherwise = Right $ account { balance = balance - amount }

    it "should inject a right value into an either" $ do
      let amount = (return 100.0) :: Either String Float
      amount `shouldBe` Right 100.0

    it "should apply a function on the right value of an either that outputs another either value" $ do
      let account = (return Account {balance = 0.0, owner = "Jane Doe"}) :: Either String Account
      account >>= (deposit 100) `shouldBe` (Right $ Account {balance = 100.0, owner = "Jane Doe"})
      account >>= (withdraw 100) `shouldBe` Left "not-enough-balance"

    it "should create a workflow with functions that either fail or success (using do-notation)" $ do
      let account = createAccount "Jane Doe"
      let result = do
                   account' <- deposit 100 account
                   account'' <-  withdraw 50 account'
                   account'''<- deposit 500 account''
                   return account'''
      result `shouldBe` (Right $ Account {balance = 550.0, owner = "Jane Doe"})
