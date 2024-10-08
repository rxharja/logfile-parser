{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Text.RawString.QQ ( r )
import Text.Trifecta 
import Data.ActivityLog
import Parser.LogFile
import qualified ParserTests (main)
import qualified DataTests (main)

testLogfile :: String
testLogfile = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep 

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast-- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

test :: IO ()
test = do
  let logfile = parseString parseLogFile mempty testLogfile  
  print logfile
  print $ averageTimeByActivity . toActivityLog <$> logfile

main :: IO ()
main = do
  ParserTests.main
  DataTests.main
