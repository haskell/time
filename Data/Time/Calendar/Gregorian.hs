{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.Gregorian
(
	-- * Gregorian calendar
	gregorian,fromGregorian,showGregorian

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"
) where

import Data.Time.Calendar.YearDay
import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

gregorian :: Date -> (Integer,Int,Int)
gregorian date = (year,month,day) where
	(year,yd) = yearAndDay date
	(month,day) = findMonthDay (monthLengths (isLeapYear year)) yd

fromGregorian :: Integer -> Int -> Int -> Date
-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
fromGregorian year month day = ModJulianDay
	((fromIntegral (clip 1 monthLength day)) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882) where
	month' = clip 1 12 month
	month'' = fromIntegral month'
	a = div (14 - month'') 12
	y = year - a
	m = month'' + (12 * a) - 3
	monthLength = (monthLengths (isLeapYear year)) !! (month' - 1)

showGregorian :: Date -> String
showGregorian date = (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d) where
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
