{-# OPTIONS_GHC -Wno-orphans #-}
module ParserTests where

import Parser.DateTime
import Parser.LogFile
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Trifecta (parseString, Result (Success, Failure), ErrInfo (ErrInfo, _errDoc))
import Text.Trifecta.Parser (Parser)
import Data.List (isInfixOf)
import Data.Map (fromList)
import Data.Monoid (All(getAll, All))

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

instance Arbitrary Header where
  arbitrary = Header <$> arbitrary

instance Arbitrary Log where
  arbitrary = do
    let stringIsParsable = 
          getAll . mconcat . fmap All . sequence [notElem '\n', not . null, not . isInfixOf "--"]
    let arbitraryEntry = (getPrintableString <$> arbitrary) `suchThat` stringIsParsable
    Log <$> arbitrary <*> arbitraryEntry

instance Arbitrary LogFile where
  arbitrary = do
    let section' = do len <- choose (1,20)
                      logs <- vectorOf len arbitrary 
                      header <- arbitrary 
                      return (header :: Header, logs :: [Log])
  
    len <- choose (1,20)
    sections <- fromList <$> vectorOf len section'
    return $ LogFile sections
 
testParser :: (Show a, Eq a) => Parser a -> a -> Expectation
testParser parser x =
  case parseString parser mempty (show x) of
   Text.Trifecta.Success y -> y `shouldBe` x
   Text.Trifecta.Failure (ErrInfo {_errDoc=doc}) -> fail (show doc)

main :: IO ()
main = hspec $ do
  describe "Time" $ do
    prop "should successfully parse the text representation of time" $
      testParser parseTime

  describe "Date" $ do
    prop "should successfully parse the text representation of date" $
      testParser parseDate

  describe "Header" $ do
    prop "should successfully parse the text representation of a header" $
      testParser parseHeader

  describe "Log" $ do
    prop "should successfully parse the text representation of a log" $
      testParser parseLog

  describe "LogFile" $ do
    prop "should successfully parse the text representation of a logfile" $
      testParser parseLogFile
