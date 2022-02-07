{-# LANGUAGE RebindableSyntax #-}
module Monad.Transformer.EitherTSpec where

import Test.Hspec
import Monad.Transformer.EitherT
import Monad.Either
import Monad.Maybe
import Monad.IO
import Functor
import Monad
import ApplicativeFunctor
import Data.List.Split
import Prelude hiding (IO, Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum, Just, Maybe)

--MaybeT for find user in db and comibine with DBErrors

data CsvError = FileNotFound | InvalidFileFormat deriving (Eq, Show)

type Path = String

type ReadCsv = Path -> EitherT CsvError IO [[String]]

readCsvSuccessfully :: [[String]] -> ReadCsv
readCsvSuccessfully successAnswer = \path -> EitherT $ pure (Right successAnswer)

readCsv :: ReadCsv
readCsv = readCsvSuccessfully [["Jane", "Doe", "1980", "NY"]]

readCsvFailing :: CsvError -> ReadCsv
readCsvFailing failAnswer = \path -> EitherT $ pure (Left failAnswer)

readCsv' :: ReadCsv
readCsv' = readCsvFailing FileNotFound

spec :: Spec
spec = do

  describe "Running the either transformer" $ do

    it "should run a monad with error handling capabilities" $ do
      let io = (runEitherT (readCsv "/path/persons.csv")) :: IO (Either CsvError [[String]])
      unsafePerformIO io `shouldBe` Right [["Jane", "Doe", "1980", "NY"]]

  describe "Using functor instance of either transformer data type" $ do

    it "should map over a transformed monad with error handling" $ do
      let countRows matrix = length matrix
      unsafePerformIO (runEitherT (countRows <$> (readCsv "/path/persons.csv"))) `shouldBe` (Right 1)
      unsafePerformIO (runEitherT (countRows <$> (readCsv' "/path/persons.csv"))) `shouldBe` (Left FileNotFound)

  describe "Using applicative instance of either transformer data type" $ do

    it "should lift a value into a monad enriched with error handling" $ do
      let maybeWithEither = (pure "hello") :: EitherT String Maybe String
      runEitherT maybeWithEither `shouldBe` Just (Right "hello")

    it "should apply a normal function (without dealing with monads) over transformed monads with error handling capabilities" $ do
      let first = readCsv "/path/persons.csv"
      let second = readCsv "/path/persons_2.csv"
      let merge :: [[String]] -> [[String]] -> [[String]]
          merge dataset1 dataset2 = dataset1 <> dataset2
      unsafePerformIO (runEitherT (merge <$> first <*> second)) `shouldBe` Right [["Jane", "Doe", "1980", "NY"], ["Jane", "Doe", "1980", "NY"]]


  describe "Using monad instance of either transformer data type" $ do

    it "should combine computations that output transformed monads with error handling capabilities" $ do
      let numberOfPersons = readCsv "/path/persons.csv" >>= (\a -> readCsv "/path/persons2.csv" >>= (\b -> readCsv "/path/persons3.csv" $> \c -> length a + length b + length c))
      unsafePerformIO (runEitherT numberOfPersons) `shouldBe` Right 3

    it "should simplify workflows that combine transformed monads with error handling using do-notation" $ do
      let mergeCSVs from1 from2 = do
                                    x <- readCsv from1
                                    y <- readCsv from2
                                    return (x <> y)
      unsafePerformIO (runEitherT (mergeCSVs "/path/persons.csv" "/path/persons2.csv")) `shouldBe` Right [["Jane", "Doe", "1980", "NY"], ["Jane", "Doe", "1980", "NY"]]

