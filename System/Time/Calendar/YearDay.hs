{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.YearDay
	(
	-- * Year and day format
	module System.Time.Calendar.YearDay
	) where

import System.Time.Calendar.Calendar
import System.Time.Calendar.Private
import System.Time.Clock

-- | ISO 8601 Ordinal Date
data YearDay = YearDay {
	ydYear  :: Integer,
	ydDay   :: Int
} deriving (Eq,Ord)

instance Show YearDay where
	show (YearDay y d) = (show y) ++ "-" ++ (show3 d)

instance DayEncoding YearDay where
	encodeDay mjd = YearDay year yd where
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
	decodeDay (YearDay year day) =
		(fromIntegral day) + (div (1532) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		y = year - 1

isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

-- | Get the number of the Monday-starting week in the year and the day of the week.
-- The first Monday is the first day of week 1, any earlier days in the year are week 0 (as \"%W\" in formatTime).
-- Monday is 1, Sunday is 7 (as \"%u\" in formatTime).
mondayStartWeek :: ModJulianDay -> (Int,Int)
mondayStartWeek mjd =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7) + 1) where
	yd = ydDay (encodeDay mjd)
	d = mjd + 2
	k = d - (toInteger yd)

-- | Get the number of the Sunday-starting week in the year and the day of the week.
-- The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as \"%U\" in formatTime).
-- Sunday is 0, Saturday is 6 (as \"%w\" in formatTime).
sundayStartWeek :: ModJulianDay -> (Int,Int)
sundayStartWeek mjd =(fromInteger ((div d 7) - (div k 7)),fromInteger (mod d 7)) where
	yd = ydDay (encodeDay mjd)
	d = mjd + 3
	k = d - (toInteger yd)
