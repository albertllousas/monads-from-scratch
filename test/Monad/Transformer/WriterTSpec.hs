{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.WriterTSpec where

import Test.Hspec
import Monad.Transformer.WriterT
import Monad.Transformer.MaybeT
import Monad.IO
import Functor
import MonadTransformer
import Monad
import Monad.Maybe
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (IO, Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Nothing, Maybe, Monad)

data User =  User { userId::String, name :: String } deriving (Show, Eq)

saveUser :: User -> WriterT [String] IO ()
saveUser user = WriterT $ pure (["saving user " <> userId user], ())

findUser :: String -> WriterT [String] (MaybeT IO) User
findUser id = WriterT $ pure (["found user " <> id], (User {userId = "1", name = "Jane"}))

spec :: Spec
spec = do

  describe "Running the writer transformer" $ do

    it "should run a monad with additional execution log" $ do
      unsafePerformIO (runWriterT (saveUser User {userId = "1", name = "Jane"})) `shouldBe` (["saving user 1"], ())

  describe "Using functor instance of writer transformer data type" $ do

    it "should map over a transformed monad with additional execution log" $ do
      unsafePerformIO (runMaybeT (runWriterT (name <$> (findUser "1")))) `shouldBe` (Just (["found user 1"], "Jane"))

  describe "Using applicative instance of writer transformer data type" $ do

    it "should lift a value into a monad with additional execution log" $ do
      let ioWithWriter = (pure "hello") :: WriterT [String] IO String
      unsafePerformIO (runWriterT ioWithWriter) `shouldBe` ([], "hello")

    it "should apply a normal function (without dealing with monads) over transformed monads with additional execution log" $ do
      let ids u1 u2 = [(userId u1),(userId u2)]
      unsafePerformIO (runMaybeT (runWriterT (pure ids <*> (findUser "1") <*> (findUser "1")))) `shouldBe` (Just (["found user 1", "found user 1"], ["1", "1"]))


  describe "Using monad instance of writer transformer data type" $ do

    it "should combine computations that output transformed monads with accumulated logs along with the results" $ do
      let users = (findUser "1") >>= (\u1 -> findUser "1" >>= (\u2 -> findUser "1" $> \u3 -> [u1, u2, u3]))
      let user = User {userId = "1", name = "Jane"}
      unsafePerformIO (runMaybeT (runWriterT users)) `shouldBe` Just (["found user 1","found user 1","found user 1"], [user, user, user])

    it "should simplify workflows with transformed monads that accumulate the logs of the executions using do-notation" $ do
      let user = User {userId = "1", name = "Jane"}
      let writer = do
                   u1 <- findUser "1"
                   u2 <- findUser "1"
                   pure ([u1, u2])
      unsafePerformIO (runMaybeT (runWriterT writer)) `shouldBe` Just (["found user 1","found user 1"], [user, user])
