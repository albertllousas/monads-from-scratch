{-# LANGUAGE RebindableSyntax #-}
module Monad.WriterSpec where

import Test.Hspec
import Monad.Writer
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Data.List.Split
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum)

-- Calculator
sum :: (Show a, Num a) => a -> a -> Writer [String] a
sum a b = Writer ([("sum " <> show a <> " + " <> show b)], (a + b))

diff :: (Show a, Num a) => a -> a -> Writer [String] a
diff a b = Writer ([("diff " <> show a <> " - " <> show b)], (a - b))

spec :: Spec
spec = do

  describe "Running the writer" $ do

    it "should run a function that calculates a result and writes a log" $ do
      runWriter (sum 1 2) `shouldBe` (["sum 1 + 2"], 3)

  describe "Using functor instance of writer data type" $ do

    it "should apply a transformation over the result of a computation that produce a result along with a log" $ do
      runWriter ((+1) <$> (sum 1 2)) `shouldBe` (["sum 1 + 2"], 4)

  describe "Using applicative instance of writer data type" $ do

    it "should lift a value into a computation" $ do
      let writer = (pure "hello") :: Writer [String] String
      runWriter writer `shouldBe` ([], "hello")

    it "should apply a normal function (without producing logs) over the results of computations that also produce logs" $ do
      let firstSumWithLog = (sum 1 2)
      let secondSumWithLog = (sum 3 4)
      let multiplicationWithoutLog a b = a * b
      runWriter (pure multiplicationWithoutLog <*> firstSumWithLog <*> secondSumWithLog) `shouldBe` (["sum 1 + 2","sum 3 + 4"], 21)

  describe "Using monad instance of writer data type" $ do

    it "should combine two computations that are accumulating along with the results" $ do
      let writer = sum 1 2 >>= (\x -> sum x 3 )
      runWriter writer `shouldBe` (["sum 1 + 2","sum 3 + 3"], 6)

    it "should compose a workflow that accumulates the logs of the executions (using do-notation)" $ do
      let writer = do
                   x <- sum 5 5
                   y <- sum 1 2
                   diff x y
      runWriter writer `shouldBe` (["sum 5 + 5","sum 1 + 2","diff 10 - 3"],7)

    it "should be able to write additional logs in a simple way inside a computation (using tell function and do-notation)" $ do
      let writer = do
                   x <- sum 5 5
                   y <- sum 1 2
                   _ <- tell [("mul " <> show x <> " * " <> show y)]
                   return (x * y) -- it's a flatmap
      runWriter writer `shouldBe` (["sum 5 + 5","sum 1 + 2","mul 10 * 3"], 30)
