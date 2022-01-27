{-# LANGUAGE RebindableSyntax #-}
module Monad.IOSpec where

import Test.Hspec
import Functor
import ApplicativeFunctor
import Monad
import Monad.IO
import Data.Char (toUpper)
import Prelude hiding (Functor, fmap, IO, (<*>), pure, Monad, return, (>>=))

-- fake read line
readLine :: String -> IO String
readLine expectedResult = IO (\_ -> (expectedResult, ()))

putLine :: String -> IO ()
putLine line = IO (\_ -> ((), ()))

toUppercase s = map toUpper s

spec :: Spec
spec = do
--https://livebook.manning.com/book/get-programming-with-haskell/chapter-41/44
  describe "Using functor instance of IO data type" $ do

    it "should apply a pure function (side-effects free) over an I/O action" $ do
      let line = readLine "crazy stuff"
      unsafePerformIO (fmap toUppercase line) `shouldBe` "CRAZY STUFF"

  describe "Using applicative instance of maybe data type" $ do

    it "should lift a value into an IO action" $ do
      unsafePerformIO (pure "hello") `shouldBe` "hello"

    it "should allow to apply a 'normal' function (taking non-IO args) on several I/O actions" $ do
      let firstName = readLine "Jane"
      let middleName = readLine "Smith"
      let lastName = readLine "Doe"
      let fullName :: String -> String -> String -> String
          fullName f m s = f <> " " <> m <> " " <> s
      unsafePerformIO (pure fullName <*> firstName <*> middleName <*> lastName) `shouldBe` "Jane Smith Doe"

  describe "Using monad instance of IO data type" $ do

    it "should inject a right value into an IO" $ do
      unsafePerformIO (return "hello") `shouldBe` "hello"

    it "should apply a function on an input/output action that performs another input/output" $ do
      unsafePerformIO (readLine "Jane" >>= (\name -> readLine "Doe" $> (\surname -> name <> " " <> surname))) `shouldBe` "Jane Doe"

    it "should compose a workflow with input/output operations (using do-notation)" $ do
      let result = do
                   _ <- putLine "Insert name"
                   firstName <- readLine "Jane"
                   _ <- putLine "Insert middle name"
                   middleName <-  readLine "Smith"
                   _ <- putLine "Insert last name"
                   lastName <- readLine "Doe"
                   return (firstName <> " " <> middleName <> " " <> lastName)
      unsafePerformIO result `shouldBe` "Jane Smith Doe"
