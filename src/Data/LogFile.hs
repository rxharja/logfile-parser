{-# LANGUAGE OverloadedStrings #-}

module Data.LogFile where
import Data.Map (Map)
import qualified Data.Map as M
import Text.Trifecta
import Control.Applicative 
import Data.DateTime

type Entry = String

data Log = Log { logTime :: Time, logEntry :: Entry } deriving (Ord, Eq)

instance Show Log where
  show (Log t e) = show t ++ " " ++ e

type Section = (Date, [Log])

newtype LogFile = LogFile { logFile :: Map Date [Log] } deriving (Ord, Eq)

instance Show LogFile where
  show (LogFile m) = ("\n" ++) . unlines . map showSection . M.toList $ m
    where
      showSection (d,t) = unlines (("# " ++ show d): map show t)

combineAllTimes :: [Log] -> Total
combineAllTimes = mconcat . computeTimeSpent . map (Total . logTime)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany (char ' ')

parseComment :: Parser String
parseComment = string "--" >> many (noneOf "\n")

skipRest' :: Parser ()
skipRest' = skipWhiteSpace >> skipOptional parseComment >> skipEOL

parseRest :: Parser String
parseRest = many (char ' ') <|> parseComment <|> many (oneOf "")

parseEnd :: Parser String
parseEnd = try parseComment <|> try (some (oneOf "\n")) <|> (eof >> return mempty)

skipRest :: Parser ()
skipRest = try (skipMany parseEnd) <|> eof

parseEntry :: Parser Entry
parseEntry = manyTill anyChar parseEnd

parseLog :: Parser Log
parseLog = Log <$> parseTime <* char ' ' <*> parseEntry <* skipRest

parseSection :: Parser Section
parseSection = (,) <$> parseDate <* skipRest' <*> some parseLog

parseLogFile :: Parser LogFile
parseLogFile = (LogFile . M.fromList) <$ many parseEnd <*> many parseSection
