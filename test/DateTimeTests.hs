{-# OPTIONS_GHC -Wno-orphans #-}
module DateTimeTests where

import Data.DateTime
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.Hspec
import Test.QuickCheck.Classes
import Test.Hspec.QuickCheck (prop)
import Text.Trifecta (parseString, Result (Success, Failure), ErrInfo (ErrInfo, _errDoc))
import Text.Trifecta.Parser (Parser)

instance Arbitrary Date where
  arbitrary = do
    year' <- choose (1900, 2100)
    month' <- choose (1, 12)
    day' <- choose (1, 31)
    return $ Date year' month' day'

instance Arbitrary Time where
  arbitrary = do
    hours' <- choose (0, 23)
    minutes' <- choose (0, 59)
    return $ Time hours' minutes'

instance Arbitrary Total where
  arbitrary = Total <$> arbitrary

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary <*> arbitrary

instance EqProp Total where
  (=-=) = eq

quickBatchSpec :: TestBatch -> Spec
quickBatchSpec (name, tests) = describe name $ mapM_ (uncurry prop) tests

testParser :: (Show a, Eq a) => Parser a -> a -> Expectation
testParser parser x = 
  case parseString parser mempty ('#':show x) of 
   Text.Trifecta.Success y -> y `shouldBe` x
   Text.Trifecta.Failure (ErrInfo {_errDoc=doc}) -> fail (show doc)

main :: IO ()
main = hspec $ do
  describe "Total" $ do
    -- we can't convert rationals to the time format and return it back
    prop "Converting from Total to Rational is isomorphic" $ 
      \x -> (rationalToTotal . totalToRational) x == x

    quickBatchSpec $ semigroup (undefined :: (Total, Int))
    quickBatchSpec $ monoid (undefined :: (Total, String))

  describe "Time" $ do
    prop "should successfully parse the text representation of time" $ 
      \x -> testParser parseTime x

  describe "Date" $ do
    prop "should successfully parse the text representation of date" $ 
      \x -> testParser parseDate x
