{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.ReaderTSpec where

import Test.Hspec
import Monad.Transformer.ReaderT
import Monad.Either
import Monad.Maybe
import Monad.IO
import Functor
import MonadTransformer
import Monad
import Data.Map as Map
import ApplicativeFunctor
import Data.List.Split
import qualified Prelude as P
import Prelude hiding (IO, Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Nothing, Maybe, Monad)

--https://blog.ssanj.net/posts/2018-01-12-stacking-the-readert-writert-monad-transformer-stack-in-haskell.html

type Config = Map String String

dbConfig :: Config
dbConfig = Map.fromList [("host", "localhost"), ("port", "5432"), ("db", "test"), ("user", "7654"), ("pwd", "7654")]

getProperty :: String -> ReaderT Config Maybe String
getProperty key = ReaderT $ \config ->case Map.lookup key config of P.Nothing -> Nothing; (P.Just a) -> Just a


spec :: Spec
spec = do

  describe "Running the maybe transformer" $ do

    it "should run a monad with that has a dependency to be computed" $ do
      runReaderT (getProperty "host") dbConfig `shouldBe` (Just "localhost")

  describe "Using functor instance of maybe transformer data type" $ do

    it "should map over a transformed monad with a dependency" $ do
      runReaderT ( (\host -> "Connecting to: " <> host) <$> (getProperty "host")) dbConfig `shouldBe` (Just "Connecting to: localhost")

  describe "Using applicative instance of maybe transformer data type" $ do

    it "should lift a value into a monad with a dependency" $ do
      let ioWithReader = (pure "hello") :: ReaderT String IO String
      unsafePerformIO (runReaderT ioWithReader "whatever") `shouldBe` "hello"

    it "should apply a normal function (without dealing with monads) over transformed monads with a shared dependency" $ do
      let buildUrl host port = "https://"<>host<>":"<>port
      runReaderT (pure buildUrl <*> (getProperty "host") <*> (getProperty "port")) dbConfig `shouldBe` (Just "https://localhost:5432")


  describe "Using monad instance of either transformer data type" $ do

    it "should combine computations that output transformed monads with a shared dependency" $ do
      let message = getProperty "host" >>= (\host -> getProperty "port" $> \port -> "DB Config => "<>host<>":"<>port)
      runReaderT message dbConfig `shouldBe` (Just "DB Config => localhost:5432")

    it "should simplify workflows that combine transformed monads with error handling using do-notation" $ do
      let connectionParams = do
                    host <- getProperty "host"
                    port <- getProperty "port"
                    user <- getProperty "user"
                    pwd <- getProperty "pwd"
                    return (host, port, user, pwd)
      runReaderT connectionParams dbConfig `shouldBe` (Just ("localhost","5432","7654","7654"))
