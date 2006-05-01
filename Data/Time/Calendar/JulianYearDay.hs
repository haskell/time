{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Calendar.JulianYearDay
	(
	-- * Year and day format
	module Data.Time.Calendar.JulianYearDay
	) where

import Data.Time.Calendar.Days
import Data.Time.Calendar.Private

-- | convert to ISO 8601 Ordinal Day format. First element of result is year (proleptic Gregoran calendar),
-- second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
toJulianYearAndDay :: Day -> (Integer,Int)
toJulianYearAndDay (ModifiedJulianDay mjd) = (year,yd) where
	a = mjd + 678577
	quad = div a 1461
	d = mod a 1461
	y = min (div d 365) 3
	yd = fromInteger (d - (y * 365) + 1)
	year = quad * 4 + y + 1

-- | convert from ISO 8601 Ordinal Day format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
fromJulianYearAndDay :: Integer -> Int -> Day
fromJulianYearAndDay year day = ModifiedJulianDay mjd where
	y = year - 1
	mjd = (fromIntegral (clip 1 (if isJulianLeapYear year then 366 else 365) day)) + (365 * y) + (div y 4) - 678578

-- | show in ISO 8601 Ordinal Day format (yyyy-ddd)
showJulianYearAndDay :: Day -> String
showJulianYearAndDay date = (show4 y) ++ "-" ++ (show3 d) where
	(y,d) = toJulianYearAndDay date

-- | Is this year a leap year according to the propleptic Gregorian calendar?
isJulianLeapYear :: Integer -> Bool
isJulianLeapYear year = (mod year 4 == 0)
