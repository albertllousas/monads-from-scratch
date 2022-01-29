{-# LANGUAGE RebindableSyntax #-}
module Monad.StateSpec where

import Test.Hspec
import Monad.State
import Functor
import Monad
import ApplicativeFunctor
import Data.Char (toUpper)
import Data.List.Split
import Prelude hiding (Either, Right, Left, (<$>), (<*>), fmap, pure, return, (>>=), sum)


--https://en.wikipedia.org/wiki/Odds_and_evens_(hand_game)#:~:text=Odds%20and%20evens%20is%20a,their%20closed%20hand%2C%20while%20another
--https://hugopeters.me/posts/13/
--https://wiki.haskell.org/State_Monad
--http://learnyouahaskell.com/for-a-few-monads-more
--https://mmhaskell.com/monads/state
--https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
--superheroes team??

--https://vmayakumar.wordpress.com/2015/12/31/magic-of-state-monad/
--https://www.slideshare.net/pjschwarz/state-monad-212839068
---- Calculator
--sum :: (Show a, Num a) => a -> a -> Writer [String] a
--sum a b = Writer ([("sum " <> show a <> " + " <> show b)], (a + b))
--
--diff :: (Show a, Num a) => a -> a -> Writer [String] a
--diff a b = Writer ([("diff " <> show a <> " - " <> show b)], (a - b))
--
--spec :: Spec
--spec = do
--
--https://en.wikipedia.org/wiki/Matching_pennies

-- Matching pennies game
-- https://en.wikipedia.org/wiki/Matching_pennies

data Winner = Player1 | Player2 deriving (Eq, Show)

data CoinSide = Tails | Heads deriving (Eq, Show)

type GameState = (Int, Int)

playMatchingPennies:: CoinSide -> CoinSide -> State GameState Winner
playMatchingPennies Tails Tails = State (\(p1, p2) -> ((p1 + 1, p2), Player1))
playMatchingPennies Heads Heads = State (\(p1, p2) -> ((p1 + 1, p2), Player1))
playMatchingPennies _ _ = State (\(p1, p2) -> ((p1, p2 + 1), Player2))

spec :: Spec
spec = do

  describe "Running the state" $ do

    it "should run a function that consume a state and produce both a result and an updated state (a pure stateful computation)" $ do
      runState (playMatchingPennies Heads Heads) (0,0) `shouldBe` ((1,0), Player1)

  describe "Using functor instance of state data type" $ do

    it "should apply a transformation over the result of an stateful computation" $ do
      let state = playMatchingPennies Heads Heads
      let nameWinner :: String -> String -> Winner -> String
          nameWinner name _ Player1 = "Amazing " <> name <>", you won this round"
          nameWinner _ name Player2 = "Stunning " <> name <>", you won this round"
      runState ((nameWinner "Jane" "John") <$> state) (0,0) `shouldBe` ((1,0), "Amazing Jane, you won this round")

  describe "Using applicative instance of state data type" $ do

    it "should lift a value into an stateful computation" $ do
      let state = (pure "hello") :: State () String
      runState state () `shouldBe` ((), "hello")

    it "should apply a normal function (without dealing with state) over the results of several stateful computations" $ do
      let firstRound = playMatchingPennies Heads Heads
      let secondRound = playMatchingPennies Tails Heads
      let thirdRound = playMatchingPennies Tails Tails
      let summaryOfLastRounds :: Winner -> Winner -> Winner -> String
          summaryOfLastRounds Player1 Player1 Player1 = "Player 1 is unstoppable"
          summaryOfLastRounds Player2 Player2 Player2 = "Player 2 is unstoppable"
          summaryOfLastRounds _ _ _ = "Both players are playing well"
      runState (pure summaryOfLastRounds <*> firstRound <*> secondRound <*> thirdRound) (0,0) `shouldBe` ((2,1),"Both players are playing well")

  describe "Using monad instance of state data type" $ do

    it "should combine two stateful computations" $ do
      let state = playMatchingPennies Heads Heads >>= (\_ -> playMatchingPennies Tails Tails)
      runState state (0,0) `shouldBe` ((2,0),Player1)

    it "should compose a workflow combining stateful computations (using do-notation)" $ do
      let state = do
                   x <- playMatchingPennies Heads Heads
                   y <- playMatchingPennies Tails Heads
                   z <- playMatchingPennies Tails Heads
                   g <- playMatchingPennies Tails Tails
                   return [x, y, z, g]
      runState state (0,0) `shouldBe` ((2,2), [Player1,Player2,Player2,Player1])

    it "should be able to read and write the current state inside an stateful computation (using get and put function and do-notation)" $ do
      let state = do
                   state <- get
                   _<- case state of (0,0) -> put state; _ -> put (0,0)
                   x <- playMatchingPennies Heads Heads
                   y <- playMatchingPennies Tails Heads
                   z <- playMatchingPennies Tails Heads
                   playMatchingPennies Tails Tails
      runState state (10,10) `shouldBe` ((2,2), Player1)

