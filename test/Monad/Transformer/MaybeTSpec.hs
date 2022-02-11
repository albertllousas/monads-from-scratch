{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.MaybeTSpec where

import Test.Hspec
import Monad.Transformer.MaybeT
import Monad.Maybe
import Monad.IO
import Functor
import MonadTransformer
import Monad
import Data.Map as Map
import ApplicativeFunctor
import qualified Prelude as P
import Prelude hiding (IO, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Maybe, Monad, id, Nothing)

type UserId = String

data User = User { userId :: String, fullName :: String } deriving (Eq, Show)

data ContactInfo = ContactInfo { contactId :: String, email :: String } deriving (Eq, Show)

type FindUser = UserId -> MaybeT IO User

users = Map.fromList [("1", User {userId = "1", fullName= "Jane Doe"}), ("2", User {userId = "2", fullName= "John Doe"})]

contacts = Map.fromList [("1", ContactInfo {contactId = "1", email= "jane.doe@gmail.com"})]

findFrom :: Map UserId a -> UserId -> MaybeT IO a
findFrom db = \id -> case Map.lookup id db of P.Nothing -> MaybeT $ pure $ Nothing; (P.Just a) -> MaybeT $ pure $ Just a

findUser = findFrom users

findContact = findFrom contacts

spec :: Spec
spec = do

  describe "Running the maybe transformer" $ do

    it "should run a monad with the capacity to deal with optional values" $ do
      let io = runMaybeT (findUser "1")
      unsafePerformIO io `shouldBe` Just (User {userId = "1", fullName = "Jane Doe"})

  describe "Using monad transformer instance of maybe transformer data type" $ do

    it "should lift (transform) a monad to a maybe transformer context" $ do
      let transformedIO = (lift $ pure "hello") :: MaybeT IO String
      unsafePerformIO (runMaybeT transformedIO) `shouldBe` Just "hello"

  describe "Using functor instance of maybe transformer data type" $ do

    it "should map over a transformed monad with optional handling capabilities" $ do
      let io = runMaybeT (fullName <$> findUser "1")
      unsafePerformIO io `shouldBe` Just "Jane Doe"

  describe "Using applicative instance of either transformer data type" $ do

    it "should lift a value into a monad enriched with optional handling" $ do
      let ioWithMaybe = (pure "hello") :: MaybeT IO String
      unsafePerformIO (runMaybeT ioWithMaybe) `shouldBe` Just "hello"

    it "should apply a normal function (without dealing with monads) over transformed monads with error optional handling capabilities" $ do
      let jane = findUser "1"
      let john = findUser "2"

      unsafePerformIO (runMaybeT ((,) <$> jane <*> john)) `shouldBe` Just (User {userId = "1", fullName= "Jane Doe"}, User {userId = "2", fullName= "John Doe"})


  describe "Using monad instance of maybe transformer data type" $ do

    it "should combine computations that output transformed monads with optional handling capabilities" $ do
      let ioWithMaybe = findUser "1" >>= (\u -> findContact "1" $> (\c -> fullName u <> " - " <> email c))
      unsafePerformIO (runMaybeT ioWithMaybe) `shouldBe` Just "Jane Doe - jane.doe@gmail.com"


    it "should simplify workflows that combine transformed monads with error handling using do-notation" $ do
      let userInfo userId = do
                            user <- findUser "1"
                            contact <- findContact "1"
                            return (fullName user <> " - " <> email contact)
      unsafePerformIO (runMaybeT $ userInfo "1") `shouldBe` Just "Jane Doe - jane.doe@gmail.com"
