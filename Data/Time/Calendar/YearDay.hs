{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.YearDay
	(
	-- * Year and day format
	module Data.Time.Calendar.YearDay
	) where

import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

yearAndDay :: Date -> (Integer,Int)
yearAndDay (ModJulianDay mjd) = (year,yd) where
	a = mjd + 678575
	quadcent = div a 146097
	b = mod a 146097
	cent = min (div b 36524) 3
	c = b - (cent * 36524)
	quad = div c 1461
	d = mod c 1461
	y = min (div d 365) 3
	yd = fromInteger (d - (y * 365) + 1)
	year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

fromYearAndDay :: Integer -> Int -> Date
fromYearAndDay year day = ModJulianDay mjd where
	y = year - 1
	mjd = (fromIntegral day) + (div (1532) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882

-- | ISO 8601 Ordinal Date
showYearAndDay :: Date -> String
showYearAndDay date = (show y) ++ "-" ++ (show3 d) where
	(y,d) = yearAndDay date

isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

-- | Get the number of the Monday-starting week in the year and the day of the week.
-- The first Monday is the first day of week 1, any earlier days in the year are week 0 (as \"%W\" in formatTime).
-- Monday is 1, Sunday is 7 (as \"%u\" in formatTime).
mondayStartWeek :: Date -> (Int,Int)
mondayStartWeek date = (fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7) + 1) where
	yd = snd (yearAndDay date)
	d = (getModJulianDay date) + 2
	k = d - (toInteger yd)

-- | Get the number of the Sunday-starting week in the year and the day of the week.
-- The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as \"%U\" in formatTime).
-- Sunday is 0, Saturday is 6 (as \"%w\" in formatTime).
sundayStartWeek :: Date -> (Int,Int)
sundayStartWeek date =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7)) where
	yd = snd (yearAndDay date)
	d = (getModJulianDay date) + 3
	k = d - (toInteger yd)
