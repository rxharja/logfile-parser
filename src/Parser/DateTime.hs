module Parser.DateTime where

import Text.Trifecta

type Year = Int

type Month = Int

type Day = Int

type Hours = Int

type Minutes = Int

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Ord, Eq)

data Time = Time { hours :: Hours, minutes :: Minutes } deriving (Ord, Eq)

instance Show Time where
  show (Time h m) = toTwoDigit h ++ ":" ++ toTwoDigit m

instance Show Date where
  show (Date y m d) = show y ++ "-" ++ toTwoDigit m ++ "-" ++ toTwoDigit d

toTwoDigit :: (Show a, Num a, Ord a) => a -> [Char]
toTwoDigit n = if n < 10 then '0':show n else show n

parseRange :: Int -> Int -> Int -> String -> Parser Int
parseRange x start stop fail' = do
  n <- read <$> count x digit
  if n >= start && n <= stop then return n else fail fail'

parseYear :: Parser Year
parseYear = parseRange 4 1900 2100 "Years must be between 1900 and 2100"

parseMonth :: Parser Month
parseMonth = parseRange 2 1 12 "Months must be between 1 and 12"

parseDay :: Parser Day
parseDay = parseRange 2 1 31 "Days must be between 1 and 31"

parseHours :: Parser Hours
parseHours = parseRange 2 0 24 "Hours must be between 1 and 24"

parseMinutes :: Parser Minutes
parseMinutes = parseRange 2 0 60 "Hours must be between 1 and 24"

parseTime :: Parser Time
parseTime = Time <$> parseHours <* char ':' <*> parseMinutes

parseDate :: Parser Date
parseDate = Date
         <$  optional (char ' ')
         <*> parseYear <* char '-'
         <*> parseMonth <* char '-'
         <*> parseDay
