{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Gregorian
(
	GregorianDay(..),GregorianTime
) where

import System.Time.Calendar.Calendar
import System.Time.Calendar.Private
import System.Time.Clock

-- | a year, month and day aggregate, suitable for the Gregorian calendar
data GregorianDay = GregorianDay {
	cdYear    :: Integer,
	cdMonth   :: Int,
	cdDay     :: Int
} deriving (Eq,Ord)

type GregorianTime = CalendarTime GregorianDay

instance Show GregorianDay where
	show (GregorianDay y m d) = (if y > 0 then show y else (show (1 - y) ++ "BCE")) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)

-- ((365 * 3 + 366) * 24 + 365 * 4) * 3 + (365 * 3 + 366) * 25
dayToYearDay :: ModJulianDay -> (Integer,Int,Bool)
dayToYearDay mjd = (year,yd,isleap) where
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
	isleap = (y == 3) && ((cent == 3) || not (quad == 24))

findMonthDay :: [Int] -> Int -> (Int,Int)
findMonthDay (n:ns) yd | yd > n = (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1,yd)


months :: Bool -> [Int]
months isleap = 
	[31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
	--J        F                   M  A  M  J  J  A  S  O  N  D

instance Calendar GregorianDay where
	dayToCalendar mjd = GregorianDay year month day where
		(year,yd,isleap) = dayToYearDay mjd
		(month,day) = findMonthDay (months isleap) yd
	-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
	calendarToDay (GregorianDay year month day) =
		(fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		month' = fromIntegral month
		a = div (14 - month') 12
		y = year - a
		m = month' + (12 * a) - 3
	calendarToMaybeDay = Just . calendarToDay -- WRONG
	formatCharacter (GregorianDay _ _ _) _ = Nothing
