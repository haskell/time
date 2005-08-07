{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.Gregorian
(
	-- * Gregorian calendar
	toGregorian,fromGregorian,showGregorian,gregorianMonthLength

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"
) where

import Data.Time.Calendar.YearDay
import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | convert to proleptic Gregorian calendar. First element of result is year, second month number (1-12), third day (1-31).
toGregorian :: Day -> (Integer,Int,Int)
toGregorian date = (year,month,day) where
	(year,yd) = toYearAndDay date
	(month,day) = findMonthDay (monthLengths (isLeapYear year)) yd

-- | convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31).
-- Invalid values will be clipped to the correct range, month first, then day.
fromGregorian :: Integer -> Int -> Int -> Day
-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
fromGregorian year month day = ModifiedJulianDay
	(day' + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882) where
	month' = clip 1 12 month
	month'' = fromIntegral month'
	a = div (14 - month'') 12
	y = year - a
	m = month'' + (12 * a) - 3
	day' = fromIntegral (clip 1 (gregorianMonthLength' year month') day)

-- | show in ISO 8601 format (yyyy-mm-dd)
showGregorian :: Day -> String
showGregorian date = (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d) where
	(y,m,d) = toGregorian date

findMonthDay :: [Int] -> Int -> (Int,Int)
findMonthDay (n:ns) yd | yd > n = (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1,yd)

gregorianMonthLength' :: Integer -> Int -> Int
gregorianMonthLength' year month' = (monthLengths (isLeapYear year)) !! (month' - 1)

-- | The number of days in a given month according to the proleptic Gregorian calendar. First argument is year, second is month.
gregorianMonthLength :: Integer -> Int -> Int
gregorianMonthLength year month = gregorianMonthLength' year (clip 1 12 month)

monthLengths :: Bool -> [Int]
monthLengths isleap = 
	[31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
	--J        F                   M  A  M  J  J  A  S  O  N  D

instance Show Day where
	show = showGregorian
