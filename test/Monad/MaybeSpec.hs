{-# LANGUAGE RebindableSyntax #-}
module Monad.MaybeSpec where

import Test.Hspec
import Monad.Maybe
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Prelude hiding (Maybe, Just, Nothing, (<$>), (<*>), fmap, pure, return, (>>=))

spec :: Spec
spec = do

  describe "Using functor instance of maybe data type" $ do

    let toUppercase s = map toUpper s

    it "should apply a function on a non empty value" $ do
      let name = Just "jane"
      fmap toUppercase name `shouldBe` Just "JANE"
      (+1) <$> (Just 1) `shouldBe` Just 2

    it "should not apply a function on an empty value" $ do
      let name = Nothing
      fmap toUppercase name `shouldBe` Nothing

  describe "Using applicative instance of maybe data type" $ do

     it "should lift a value into a maybe" $ do
        pure "hello" `shouldBe` Just "hello"

     it "should allow to apply a 'normal' function (taking non-either args) on two optional values" $ do
       (+) <$> Just 1 <*> Just 2 `shouldBe` Just 3
       (+) <$> Just 1 <*> Nothing `shouldBe` Nothing
       (+) <$> Nothing <*> Just 2 `shouldBe` Nothing

     it "should allow to apply multiple argument function over multiple optional values" $ do
       let name = Just "Jane"
       let middleName = Just "Emily"
       let surname = Just "Doe"
       let fullName :: String -> String -> String -> String
           fullName name middleName surname = name <> " " <> middleName <> " " <> surname
       pure fullName <*> name <*> middleName <*> surname `shouldBe` Just "Jane Emily Doe" -- function gets partially applied each time
       pure fullName <*> name <*> middleName <*> Nothing `shouldBe` Nothing
       pure fullName <*> Nothing <*> middleName <*> surname `shouldBe` Nothing

  describe "Using monad instance of maybe data type" $ do

    let divide x 0 = Nothing
        divide x y = Just (x `div` y)

    it "should inject a value into a maybe" $ do
      return "hello" `shouldBe` Just "hello"

    it "should apply a function to an optional value that outputs another optional value" $ do
      divide 8 2 >>= (\z -> divide z 2) `shouldBe` Just 2
      divide 8 0 >>= (\z -> divide z 2) `shouldBe` Nothing
      divide 8 2 >>= (\z -> divide z 0) `shouldBe` Nothing

    it "should simplify a monadic composition on optionals with do notation" $ do
      let computation = divide 24 2 >>= (\x -> divide 10 5 >>= (\y -> divide 4 2 $> (\z -> (x + y + z))))
      let doComputation = do
                  x <- divide 24 2
                  y <- divide 10 5
                  z <- divide 4 2
                  return (x + y + z)
      computation `shouldBe` doComputation
