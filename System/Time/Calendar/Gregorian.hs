{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Gregorian
(
	GregorianDay(..),GregorianTime,ZonedGregorianTime

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"
) where

import System.Time.Calendar.YearDay
import System.Time.Calendar.Calendar
import System.Time.Calendar.Private

-- | a year, month and day aggregate, suitable for the Gregorian calendar
data GregorianDay = GregorianDay {
	gregYear    :: Integer,
	gregMonth   :: Int,
	gregDay     :: Int
} deriving (Eq,Ord)

type GregorianTime = DayAndTime GregorianDay

type ZonedGregorianTime = ZonedTime (DayAndTime GregorianDay)

instance Show GregorianDay where
	show (GregorianDay y m d) = (if y > 0 then show y else (show (1 - y) ++ "BCE")) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)

findMonthDay :: [Int] -> Int -> (Int,Int)
findMonthDay (n:ns) yd | yd > n = (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1,yd)


monthLengths :: Bool -> [Int]
monthLengths isleap = 
	[31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
	--J        F                   M  A  M  J  J  A  S  O  N  D

instance DayEncoding GregorianDay where
	encodeDay mjd = GregorianDay year month day where
		(YearDay year yd) = encodeDay mjd
		(month,day) = findMonthDay (monthLengths (isLeapYear year)) yd
	-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
	decodeDay (GregorianDay year month day) =
		(fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		month' = fromIntegral month
		a = div (14 - month') 12
		y = year - a
		m = month' + (12 * a) - 3
