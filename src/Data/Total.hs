module Data.Total where

import Parser.DateTime
import Control.Applicative (liftA2)

newtype Total = Total { totalTime :: Time } deriving (Ord, Eq)

instance Semigroup Total where
  t1 <> t2 = rationalToTotal (totalToRational t1 + totalToRational t2)

instance Monoid Total where
  mempty = Total (Time 0 0)

instance Show Total where
  show (Total (Time h m)) = show h ++ " hours and " ++ show m ++ " minutes."

data DateTime = DateTime { date :: Date, time :: Total } deriving (Ord, Eq)

instance Show DateTime where
  show (DateTime d t) = (tail . show) d ++ ", " ++ show t

timeSpent :: Total -> Total -> Total
timeSpent t1 t2 = rationalToTotal (totalToRational t1 - totalToRational t2) 

computeTimeSpent :: [Total] -> [Total]
computeTimeSpent = liftA2 (zipWith timeSpent) tail id

toMinutes :: Rational -> Minutes
toMinutes = truncate . (* 60)

toRational' :: Rational -> Rational
toRational' = (/ 60)

getRational :: Rational -> Rational
getRational x = x - fromInteger (truncate x)

totalToRational :: Total -> Rational
totalToRational (Total (Time h m)) = fromIntegral h + (toRational' . fromIntegral) m

rationalToTotal :: Rational -> Total
rationalToTotal x = 
  let hrs = truncate x
      mins = toMinutes . getRational $ x
   in Total $ Time hrs mins
