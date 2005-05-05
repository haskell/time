{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Gregorian
(
	GregorianDay(..),GregorianTime,ZonedGregorianTime

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"
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

type ZonedGregorianTime = ZonedTime (CalendarTime GregorianDay)

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

isoWeekFormat :: ModJulianDay -> (Integer,Int,Int)
isoWeekFormat day = (y,div k 7,fromInteger (mod day 7) + 1) where
	(year,yd,_) = dayToYearDay day
	k = yd -- WRONG
	y = year -- WRONG

instance FormatTime GregorianDay where
	formatCharacter 'a' = Just (\locale day -> snd ((wDays locale) !! (weekDay (decodeDay day))))
	formatCharacter 'A' = Just (\locale day -> fst ((wDays locale) !! (weekDay (decodeDay day))))
	formatCharacter 'b' = Just (\locale (GregorianDay _ m _) -> snd ((months locale) !! (m - 1)))
	formatCharacter 'B' = Just (\locale (GregorianDay _ m _) -> fst ((months locale) !! (m - 1)))
	formatCharacter 'C' = Just (\_ (GregorianDay y _ _) -> show2 (fromInteger (div y 100)))
	formatCharacter 'd' = Just (\_ (GregorianDay _ _ d) -> show2 d)
	formatCharacter 'D' = Just (\locale day -> formatTime locale "%m/%d/%y" day)
	formatCharacter 'e' = Just (\_ (GregorianDay _ _ d) -> show2Space d)
	formatCharacter 'F' = Just (\locale day -> formatTime locale "%Y-%m-%d" day)
	formatCharacter 'g' = Just (\_ day -> let (y,_,_) = isoWeekFormat (decodeDay day) in show2 (fromInteger (mod y 100)))
	formatCharacter 'G' = Just (\_ day -> let (y,_,_) = isoWeekFormat (decodeDay day) in show y)
	formatCharacter 'h' = Just (\locale (GregorianDay _ m _) -> snd ((months locale) !! (m - 1)))
	formatCharacter 'j' = Just (\_ day -> show3 (dayOfYear (decodeDay day)))
	formatCharacter 'm' = Just (\_ (GregorianDay _ m _) -> show2 m)
	formatCharacter 'u' = Just (\_ day -> show (weekDay' (decodeDay day)))
	formatCharacter 'U' = Just (\_ day -> show2 (weekNumber (decodeDay day)))
	formatCharacter 'V' = Just (\_ day -> let (_,n,_) = isoWeekFormat (decodeDay day) in show2 n)
	formatCharacter 'w' = Just (\_ day -> show (weekDay (decodeDay day)))
	formatCharacter 'W' = Just (\_ day -> show2 (weekNumber' (decodeDay day)))
	formatCharacter 'x' = Just (\locale day -> formatTime locale (dateFmt locale) day)
	formatCharacter 'y' = Just (\_ (GregorianDay y _ _) -> show2 (fromInteger (mod y 100)))
	formatCharacter 'Y' = Just (\_ (GregorianDay y _ _) -> show y)
	formatCharacter _   = Nothing

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

instance DayEncoding GregorianDay where
	encodeDay mjd = GregorianDay year month day where
		(year,yd,isleap) = dayToYearDay mjd
		(month,day) = findMonthDay (monthLengths isleap) yd
	-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
	decodeDay (GregorianDay year month day) =
		(fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
		month' = fromIntegral month
		a = div (14 - month') 12
		y = year - a
		m = month' + (12 * a) - 3
	maybeDecodeDay = Just . decodeDay -- WRONG
