module System.Time.Calendar
(
	-- time zones
	TimeZone,timezoneToMinutes,minutesToTimezone,

	-- getting the locale time zone

	-- converting times to Gregorian "calendrical" format
	TimeOfDay,CalendarDay,CalendarTime,
	dayToCalendar

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"

	-- parsing and showing dates and times
) where

import System.Time.Clock
import Data.Char

-- | count of minutes
newtype TimeZone = MkTimeZone {
	timezoneToMinutes :: Int
} deriving (Eq,Ord)

minutesToTimezone :: Int -> TimeZone
minutesToTimezone = MkTimeZone

-- | time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day
data TimeOfDay = TimeOfDay {
	todHour    :: Int,
	todMin     :: Int,
	todSec     :: Int,
	todPicosec :: Integer
} deriving (Eq,Ord)

show2 :: Int -> String
show2 i = let
	s = show i in
  case s of
	[_] -> '0':s
	_ -> s

showFraction :: Integer -> Integer -> String
showFraction d 0 = ""
showFraction d i = (chr (fromInteger (48 + (div i d)))):showFraction (div d 10) (mod i d)

showpicodecimal :: Integer -> String
showpicodecimal 0 = ""
showpicodecimal i = '.':(showFraction 100000000000 i)

instance Show TimeOfDay where
	show (TimeOfDay h m s ps) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2 s) ++ (showpicodecimal ps)

-- | a year, month and day aggregate, suitable for the Gregorian calendar
data CalendarDay = CalendarDay {
	cdYear    :: Integer,
	cdMonth   :: Int,
	cdDay     :: Int
} deriving (Eq,Ord)

instance Show CalendarDay where
	show (CalendarDay y m d) = (if y > 0 then show y else (show (1 - y) ++ "BCE")) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)

-- | straightforward date and time aggregate
data CalendarTime = CalendarTime {
	ctDay    :: CalendarDay,
	ctTime   :: TimeOfDay
} deriving (Eq,Ord)

instance Show CalendarTime where
	show (CalendarTime d t) = (show d) ++ " " ++ (show t)

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

-- | name the given day according to the Gregorian calendar
dayToCalendar :: ModJulianDay -> CalendarDay
dayToCalendar mjd = CalendarDay year month day where
	(year,yd,isleap) = dayToYearDay mjd
	(month,day) = findMonthDay (months isleap) yd


utcToCalendar :: TimeZone -> UTCTime -> CalendarTime
utcToCalendar tz utc = undefined

calendarToUTC :: TimeZone -> CalendarTime -> UTCTime
calendarToUTC tz cal = undefined


