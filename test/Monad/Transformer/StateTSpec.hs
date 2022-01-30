--{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.StateTSpec where

import Test.Hspec
import Monad.Transformer.StateT
import Monad.Either
import Functor
import Monad
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum)


type Change = Float

data PurchaseError = InsufficientMoney | OutOfStock deriving (Eq, Show)

data CokeVendingMachineState = CokeVendingMachineState { stock :: Int, cokePrice :: Float} deriving (Eq, Show)

buyCoke:: Float -> StateT CokeVendingMachineState (Either PurchaseError) Change
buyCoke coins = StateT $ \state -> case () of
                                        _ | (coins < cokePrice state) -> Left InsufficientMoney
                                          | (stock state == 0) -> Left OutOfStock
                                          | otherwise -> Right (state { stock = (stock state) - 1 }, coins - (cokePrice state))

initState = CokeVendingMachineState {stock = 5, cokePrice = 1.5}


spec :: Spec
spec = do

  describe "Running the state" $ do

    it "should run a function that deals with an stateful computation, wrapping the result in another monad" $ do
      runStateT (buyCoke 10) initState `shouldBe` Right (CokeVendingMachineState {stock = 4, cokePrice = 1.5}, 8.5)


