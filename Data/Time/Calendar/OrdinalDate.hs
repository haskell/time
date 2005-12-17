{-# OPTIONS -Wall -Werror #-}

-- | ISO 8601 Ordinal Date format
module Data.Time.Calendar.OrdinalDate where

import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | convert to ISO 8601 Ordinal Date format. First element of result is year (proleptic Gregoran calendar),
-- second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
toOrdinalDate :: Day -> (Integer,Int)
toOrdinalDate (ModifiedJulianDay mjd) = (year,yd) where
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

-- | convert from ISO 8601 Ordinal Date format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
fromOrdinalDate :: Integer -> Int -> Day
fromOrdinalDate year day = ModifiedJulianDay mjd where
	y = year - 1
	mjd = (fromIntegral (clip 1 (if isLeapYear year then 366 else 365) day)) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576

-- | show in ISO 8601 Ordinal Date format (yyyy-ddd)
showOrdinalDate :: Day -> String
showOrdinalDate date = (show4 y) ++ "-" ++ (show3 d) where
	(y,d) = toOrdinalDate date

-- | Is this year a leap year according to the propleptic Gregorian calendar?
isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

-- | Get the number of the Monday-starting week in the year and the day of the week.
-- The first Monday is the first day of week 1, any earlier days in the year are week 0 (as \"%W\" in formatTime).
-- Monday is 1, Sunday is 7 (as \"%u\" in formatTime).
mondayStartWeek :: Day -> (Int,Int)
mondayStartWeek date = (fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7) + 1) where
	yd = snd (toOrdinalDate date)
	d = (toModifiedJulianDay date) + 2
	k = d - (toInteger yd)

-- | Get the number of the Sunday-starting week in the year and the day of the week.
-- The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as \"%U\" in formatTime).
-- Sunday is 0, Saturday is 6 (as \"%w\" in formatTime).
sundayStartWeek :: Day -> (Int,Int)
sundayStartWeek date =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7)) where
	yd = snd (toOrdinalDate date)
	d = (toModifiedJulianDay date) + 3
	k = d - (toInteger yd)
