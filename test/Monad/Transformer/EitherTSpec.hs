{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.EitherTSpec where

import Test.Hspec
import Monad.Transformer.EitherT
import Monad.Either
import Monad.Maybe
import Monad.State
import Functor
import Monad
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (State, Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Maybe)


type Change = Float

data PurchaseError = InsufficientMoney | OutOfStock deriving (Eq, Show)

data CokeVendingMachineState = CokeVendingMachineState { stock :: Int, cokePrice :: Float} deriving (Eq, Show)

type StateWithErrors e s a = EitherT e (State s) a

buyCoke:: Float -> StateWithErrors PurchaseError CokeVendingMachineState Change
buyCoke coins = EitherT $ (State $ \s -> case () of
                                      _ | (coins < cokePrice s) -> (s, Left InsufficientMoney)
                                        | (stock s == 0) -> (s, Left OutOfStock)
                                        | otherwise -> (s { stock = (stock s) - 1 }, Right (coins - (cokePrice s))))

displayChangeMessage :: Change -> String
displayChangeMessage change = "Enjoy your drink and don't forget the change " <> show change

initState = CokeVendingMachineState { stock = 5, cokePrice = 1.5 }

spec :: Spec
spec = do

  describe "Running the either transformer" $ do

    it "should run a monad with error handling capabilities" $ do
      let stateWithErrors = (runEitherT (buyCoke 5)) :: State CokeVendingMachineState (Either PurchaseError Change)
      runState stateWithErrors initState `shouldBe` (CokeVendingMachineState {stock = 4, cokePrice = 1.5}, Right 3.5)

  describe "Using functor instance of either transformer data type" $ do

    it "should map over a transformed monad with error handling" $ do
      let vendingMachine = (displayChangeMessage <$> (buyCoke 5))
      runState (runEitherT vendingMachine) initState `shouldBe` (CokeVendingMachineState {stock = 4, cokePrice = 1.5}, Right "Enjoy your drink and don't forget the change 3.5")

  describe "Using applicative instance of either transformer data type" $ do

    it "should lift a value into a monad enriched with error handling" $ do
      let maybeWithEither = (pure "hello") :: EitherT String Maybe String
      runEitherT maybeWithEither `shouldBe` Just (Right "hello")

    it "should apply a normal function (without dealing with monads) over transformed monads with error handling capabilities" $ do
      let first = buyCoke 5
      let second = buyCoke 5
      let combineChanges :: Change -> Change -> Change
          combineChanges c1 c2 = c1 + c2
      runState (runEitherT (combineChanges <$> first <*> second)) initState `shouldBe` (CokeVendingMachineState {stock = 3, cokePrice = 1.5}, Right 7.0)


