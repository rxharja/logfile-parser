{-# LANGUAGE OverloadedStrings #-}

module Data.LogFile where
import Data.Map (Map)
import qualified Data.Map as M
import Text.Trifecta
import Control.Applicative ( Alternative((<|>), many), Applicative (liftA2) )
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

newtype ActivityLog a = ActivityLog { activityLog :: Map Entry [a] } deriving (Ord, Eq)

instance Show a => Show (ActivityLog a) where
  show (ActivityLog m) = ("\n" ++) . unwords . map showSection . M.toList $ m
    where
      showSection (e,xs) = unlines $ map (\x -> show e ++ ": " ++ show x) xs

newtype ActivitySummary a = ActivitySummary { activitySummary :: Map Entry a } deriving (Ord, Eq)

instance Show a => Show (ActivitySummary a) where
  show (ActivitySummary m) = ("\n" ++) . unlines . map showSection . M.toList $ m
    where
      showSection (e,x) =  show e ++ ": " ++ show x

test :: Log -> Log -> (Entry, Total)
test (Log t entry) (Log t' _) = (entry, timeSpent (Total t') (Total t))


combineAllTimes :: [Log] -> Total
combineAllTimes = mconcat . computeTimeSpent . map (Total . logTime)

totalTimeByActivity :: ActivityLog DateTime -> ActivitySummary Total
totalTimeByActivity (ActivityLog m) = ActivitySummary
  (M.map (decimalToTotal. sum . map (totalToDecimal . time)) m)

averageTimeByActivity :: ActivityLog DateTime -> ActivitySummary Total
averageTimeByActivity (ActivityLog m) = ActivitySummary avg
  where
    length' = M.map (fromIntegral . length) m  :: Map Entry Double
    total = M.map (sum . map (totalToDecimal . time)) m :: Map Entry Double
    avg = M.map decimalToTotal $ M.unionWith (\l t -> t / l) length' total

toActivityLog :: LogFile -> ActivityLog DateTime
toActivityLog (LogFile m) = ActivityLog $ M.foldrWithKey aggregateActivities M.empty m
    where
      addLogEntry d (Log t e) = M.insertWith (++) e [DateTime d (Total t)]
      calcTimeSpent (Log t _) (Log t' e) = Log (totalTime $ Total t `timeSpent` Total t') e
      aggregateActivities d logs acc = foldr (addLogEntry d) acc logs'
        where 
          logs' = liftA2 (zipWith calcTimeSpent) tail id logs

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
