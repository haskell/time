{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Gregorian
(
	GregorianDay(..),GregorianTime
) where

import System.Time.Calendar.Calendar
import System.Time.Calendar.Format
import System.Time.Calendar.Private
import System.Time.Clock
import System.Locale

-- | a year, month and day aggregate, suitable for the Gregorian calendar
data GregorianDay = GregorianDay {
	cdYear    :: Integer,
	cdMonth   :: Int,
	cdDay     :: Int
} deriving (Eq,Ord)

type GregorianTime = CalendarTime GregorianDay

instance Show GregorianDay where
	show (GregorianDay y m d) = (if y > 0 then show y else (show (1 - y) ++ "BCE")) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)

weekDay :: ModJulianDay -> Int
weekDay day = fromInteger (mod (day + 3) 7)

weekDay' :: ModJulianDay -> Int
weekDay' day = weekDay (day - 1) + 1

dayOfYear :: ModJulianDay -> Int
dayOfYear day = yd where
	(_,yd,_) = dayToYearDay day

weekNumber :: ModJulianDay -> Int
weekNumber day = (div (dayOfYear day) 7) + 1

weekNumber' :: ModJulianDay -> Int
weekNumber' day = (div (dayOfYear day) 7) + 1

weekNumber'' :: ModJulianDay -> Int
weekNumber'' day = (div (dayOfYear day) 7) + 1

instance FormatTime GregorianDay where
	formatCharacter locale 'a' day = Just (snd ((wDays locale) !! (weekDay (calendarToDay day))))
	formatCharacter locale 'A' day = Just (fst ((wDays locale) !! (weekDay (calendarToDay day))))
	formatCharacter locale 'b' (GregorianDay _ m _) = Just (snd ((months locale) !! (m - 1)))
	formatCharacter locale 'B' (GregorianDay _ m _) = Just (fst ((months locale) !! (m - 1)))
	formatCharacter _ 'C' (GregorianDay y _ _) = Just (show2 (fromInteger (div y 100)))
	formatCharacter _ 'd' (GregorianDay _ _ d) = Just (show2 d)
	formatCharacter locale 'D' day = Just (formatTime locale "%m/%d/%y" day)
	formatCharacter _ 'e' (GregorianDay _ _ d) = Just (show2Space d)
	formatCharacter locale 'h' (GregorianDay _ m _) = Just (snd ((months locale) !! (m - 1)))
	formatCharacter _ 'j' day = Just (show3 (dayOfYear (calendarToDay day)))
	formatCharacter _ 'm' (GregorianDay _ m _) = Just (show2 m)
	formatCharacter _ 'u' day = Just (show (weekDay' (calendarToDay day)))
	formatCharacter _ 'U' day = Just (show2 (weekNumber (calendarToDay day)))
	formatCharacter _ 'V' day = Just (show2 (weekNumber'' (calendarToDay day)))
	formatCharacter _ 'w' day = Just (show (weekDay (calendarToDay day)))
	formatCharacter _ 'W' day = Just (show2 (weekNumber' (calendarToDay day)))
	formatCharacter locale 'x' day = Just (formatTime locale (dateFmt locale) day)
	formatCharacter _ 'y' (GregorianDay y _ _) = Just (show2 (fromInteger (mod y 100)))
	formatCharacter _ 'Y' (GregorianDay y _ _) = Just (show y)
	formatCharacter _ _ _ = Nothing

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


monthLengths :: Bool -> [Int]
monthLengths isleap = 
	[31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
	--J        F                   M  A  M  J  J  A  S  O  N  D

instance Calendar GregorianDay where
	dayToCalendar mjd = GregorianDay year month day where
		(year,yd,isleap) = dayToYearDay mjd
		(month,day) = findMonthDay (monthLengths isleap) yd
	-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
	calendarToDay (GregorianDay year month day) =
		(fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		month' = fromIntegral month
		a = div (14 - month') 12
		y = year - a
		m = month' + (12 * a) - 3
	calendarToMaybeDay = Just . calendarToDay -- WRONG
