{-# LANGUAGE RebindableSyntax #-}
module Monad.ListSpec where

import Test.Hspec
import Monad.List
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Prelude hiding (List, Nil, (<$>), (<*>), fmap, pure, return, (>>=))

spec :: Spec
spec = do

  describe "Using functor instance of list data type" $ do

    it "should apply a function on each element of a list" $ do
      let list = Cons 1 (Cons 2 (Cons 3 Nil))
      fmap (+1) list `shouldBe` Cons 2 (Cons 3 (Cons 4 Nil))


  describe "Using applicative instance of list data type" $ do

    it "should lift a value into a list" $ do
      let list = (pure 1) :: List Int
      list `shouldBe` (Cons 1 Nil)

    it "should apply a list of functions to a list of values" $ do
      let functions = (Cons (+1) (Cons (*2) Nil))
      let values = (Cons 1 (Cons 2 (Cons 3 Nil)))
      functions <*> values `shouldBe` (Cons 2 (Cons 3 (Cons 4 (Cons 2 (Cons 4 (Cons 6 Nil))))))

  describe "Using monad instance of list data type" $ do

    it "should apply a function on each value of a list that outputs another list and flatten the resulting matrix" $ do
      let list = Cons 1 (Cons 2 (Cons 3 Nil))
      let repeat :: a -> Int -> List a
          repeat elem 0 = Nil
          repeat elem n = Cons elem (repeat elem (n - 1))
      list >>= (\repetitions -> repeat repetitions repetitions) `shouldBe` (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 3 (Cons 3 Nil))))))

    it "should simplify complex monadic function composition using do-notation on lists" $ do
      let xs = Cons 1 (Cons 2 Nil)
      let ys = Cons 'a' (Cons 'b' Nil)
      let result = do
                   x <- xs
                   y <- ys
                   return (x, y)
      result `shouldBe` Cons (1,'a') (Cons (1,'b') (Cons (2,'a') (Cons (2,'b') Nil)))
