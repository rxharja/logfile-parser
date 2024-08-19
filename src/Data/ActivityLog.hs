module Data.ActivityLog where

import Data.DateTime
import Data.LogFile
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Base (Applicative(liftA2))

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

totalTimeByActivity :: ActivityLog DateTime -> ActivitySummary Total
totalTimeByActivity (ActivityLog m) = ActivitySummary
  (M.map (rationalToTotal. sum . map (totalToRational . time)) m)

averageTimeByActivity :: ActivityLog DateTime -> ActivitySummary Total
averageTimeByActivity (ActivityLog m) = ActivitySummary avg
  where
    length' = M.map (fromIntegral . length) m  :: Map Entry Rational
    total = M.map (sum . map (totalToRational . time)) m :: Map Entry Rational
    avg = M.map rationalToTotal $ M.unionWith (\l t -> t / l) length' total

toActivityLog :: LogFile -> ActivityLog DateTime
toActivityLog (LogFile m) = ActivityLog $ M.foldrWithKey aggregateActivities M.empty m
    where
      addLogEntry d (Log t e) = M.insertWith (++) e [DateTime d (Total t)]
      calcTimeSpent (Log t _) (Log t' e) = Log (totalTime $ Total t `timeSpent` Total t') e
      aggregateActivities d logs acc = foldr (addLogEntry d) acc logs'
        where 
          logs' = liftA2 (zipWith calcTimeSpent) tail id logs
