{-# LANGUAGE RebindableSyntax #-}
module Monad.ReaderSpec where

import Test.Hspec
import Monad.Reader
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Data.List.Split
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=))

type Delimiter = String

toUppercase s = map toUpper s

parseValues :: String -> Reader Delimiter [String]
parseValues string = Reader $ \delimiter -> splitOn delimiter string

spec :: Spec
spec = do

  describe "Running the reader" $ do

    it "should run a function that depends on reading from a dependency (environment)" $ do
      (runReader (parseValues "Jane::Doe") "::") `shouldBe` ["Jane", "Doe"]

  describe "Using functor instance of reader data type" $ do

    it "should apply a transformation over the result of a computation that has a dependency" $ do
      let reader = (parseValues "Jane::Doe")
      runReader (map toUppercase <$> reader) "::" `shouldBe` ["JANE","DOE"]

  describe "Using applicative instance of reader data type" $ do

    it "should lift a value into a dependant computation" $ do
      runReader (pure "hello") "whatever" `shouldBe` "hello"

    it "should allow to apply a function on the results of several computations that share a common dependency" $ do
      let superman = parseValues "Clark, Kent"
      let batman = parseValues "Bruce, Wayne"
      let spiderman = parseValues "Peter, Parker"
      let welcomeSuperHeroes ::[String] ->[String] ->[String] -> String
          welcomeSuperHeroes l1 l2 l3 = "Welcome "<> head l1 <> ", " <> head l2 <> " and " <> head l3 <> " to the team!"
      runReader (pure welcomeSuperHeroes <*> superman <*> batman <*> spiderman) ", " `shouldBe` "Welcome Clark, Bruce and Peter to the team!"

  describe "Using monad instance of reader data type" $ do

    it "should combine two computations that depend on a shared dependency" $ do
      let reader = parseValues "Clark, Kent" >>= (\superman -> parseValues "Bruce, Wayne" $> (\batman -> (head superman, head batman)))
      runReader  reader ", " `shouldBe` ("Clark", "Bruce")

    it "should simplify workflows with functions that have a common dependency using do-notation" $ do
      let reader = do
                   superman <- parseValues "Clark, Kent"
                   batman <- parseValues "Bruce, Wayne"
                   spiderman <- parseValues "Peter, Parker"
                   return ("Welcome "<> head superman <> ", " <> head batman <> " and " <> head spiderman <> " to the team!")
      runReader reader ", " `shouldBe` "Welcome Clark, Bruce and Peter to the team!"

    it "should be able to access to the dependency in a simple way inside a computation (using ask function and do-notation)" $ do
      let parseCsvReader :: String -> Reader Delimiter [String]
          parseCsvReader line = do
                              delimiter <- ask
                              return (splitOn delimiter line)
      runReader (parseCsvReader "1997,Ford,E350") "," `shouldBe` ["1997","Ford","E350"]
