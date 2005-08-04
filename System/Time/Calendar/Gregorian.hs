{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.Gregorian
(
	-- * Gregorian calendar
	gregorian,fromGregorian,showGregorian

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"
) where

import System.Time.Calendar.YearDay
import System.Time.Calendar.Days
import System.Time.Calendar.Private

gregorian :: Date -> (Integer,Int,Int)
gregorian date = (year,month,day) where
	(year,yd) = yearAndDay date
	(month,day) = findMonthDay (monthLengths (isLeapYear year)) yd

fromGregorian :: Integer -> Int -> Int -> Date
-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
fromGregorian year month day = ModJulianDay
	((fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882) where
	month' = fromIntegral month
	a = div (14 - month') 12
	y = year - a
	m = month' + (12 * a) - 3

showGregorian :: Date -> String
showGregorian date = (if y > 0 then show y else (show (1 - y) ++ "BCE")) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d) where
	(y,m,d) = gregorian date

findMonthDay :: [Int] -> Int -> (Int,Int)
findMonthDay (n:ns) yd | yd > n = (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1,yd)

monthLengths :: Bool -> [Int]
monthLengths isleap = 
	[31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
	--J        F                   M  A  M  J  J  A  S  O  N  D

instance Show Date where
	show = showGregorian
