{-# OPTIONS_GHC -Wno-orphans #-}
module DataTests where

import ParserTests ()
import Data.Total
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.Hspec
import Test.QuickCheck.Classes
import Test.Hspec.QuickCheck (prop)

instance Arbitrary Total where
  arbitrary = Total <$> arbitrary

instance EqProp Total where
  (=-=) = eq

quickBatchSpec :: TestBatch -> Spec
quickBatchSpec (name, tests) = describe name $ mapM_ (uncurry prop) tests

main :: IO ()
main = hspec $ do
  describe "Total" $ do
    quickBatchSpec $ semigroup (undefined :: (Total, Int))
    quickBatchSpec $ monoid (undefined :: (Total, String))

    -- we can't convert rationals to the time format and return it back
    prop "Converting from Total to Rational is isomorphic" $ 
      \x -> (rationalToTotal . totalToRational $ x) == x
