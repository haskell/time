{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar
(
	-- time zones
	TimeZone,timezoneToMinutes,minutesToTimezone,hoursToTimezone,utc,

	-- getting the locale time zone

	-- Gregorian "calendrical" format
	TimeOfDay(..),CalendarDay(..),CalendarTime(..),
	dayToCalendar,calendarToDay,

	-- converting UTC times to Gregorian "calendrical" format
	utcToLocalTimeOfDay,localToUTCTimeOfDay,
	timeToTimeOfDay,timeOfDayToTime,
	utcToCalendar,calendarToUTC,

	-- converting UT1 times to Gregorian "calendrical" format
	ut1ToCalendar,calendarToUT1

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

hoursToTimezone :: Int -> TimeZone
hoursToTimezone i = minutesToTimezone (60 * i)

-- | The UTC time zone
utc :: TimeZone
utc = minutesToTimezone 0

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
showFraction _ 0 = ""
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

-- | find out which day a given Gregorian calendar day is
calendarToDay :: CalendarDay -> ModJulianDay
-- formula from <http://en.wikipedia.org/wiki/Julian_Day>
calendarToDay (CalendarDay year month day) =
	(fromIntegral day) + (div (153 * m + 2) 5) + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678882 where
	month' = fromIntegral month
	a = div (14 - month') 12
	y = year - a
	m = month' + (12 * a) - 3

-- | convert a ToD in UTC to a ToD in some timezone, together with a day adjustment
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay (MkTimeZone tz) (TimeOfDay h m s p) = (fromIntegral (div h' 24),TimeOfDay (mod h' 60) (mod m' 60) s p) where
	m' = m + tz
	h' = h + (div m' 60)

-- | convert a ToD in some timezone to a ToD in UTC, together with a day adjustment
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay (MkTimeZone tz) = utcToLocalTimeOfDay (MkTimeZone (negate tz))

-- note: this is also in System.Time.Clock.
posixDaySeconds :: Rational
posixDaySeconds = 86400

posixDay :: DiffTime
posixDay = siSecondsToTime posixDaySeconds

-- | get a TimeOfDay given a time since midnight
-- | time more than 24h will be converted to leap-seconds
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDay = TimeOfDay 23 59 (60 + s) p where
	offset = dt - posixDay
	s = fromIntegral (div offset siSecond)
	p = fromIntegral (mod offset siSecond)
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) (fromInteger s) p where
	p = fromIntegral (mod dt siSecond)
	s' = fromIntegral (div dt siSecond)
	s = mod s' 60
	m' = div s' 60
	m = mod m' 60
	h = div m' 60

-- | find out how much time since midnight a given TimeOfDay is
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s ps) = (((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (fromIntegral s)) * siSecond + (fromIntegral ps)

utcToCalendar :: TimeZone -> UTCTime -> CalendarTime
utcToCalendar tz (UTCTime day dt) = CalendarTime (dayToCalendar (day + i)) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

calendarToUTC :: TimeZone -> CalendarTime -> UTCTime
calendarToUTC tz (CalendarTime cday tod) = UTCTime (day + i) (timeOfDayToTime todUTC) where
	day = calendarToDay cday
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | get a TimeOfDay given the fraction of a day since midnight
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (siSecondsToTime (round (df * posixDaySeconds) :: Integer))

-- | 1st arg is observation meridian in degrees, positive is East
ut1ToCalendar :: Rational -> ModJulianDate -> CalendarTime
ut1ToCalendar long date = CalendarTime (dayToCalendar localDay) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localDay = floor localTime
	localToDOffset = localTime - (fromIntegral localDay)

-- | get the fraction of a day since midnight given a TimeOfDay
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = timeToSISeconds (timeOfDayToTime tod) / posixDaySeconds
	
-- | 1st arg is observation meridian in degrees, positive is East
calendarToUT1 :: Rational -> CalendarTime -> ModJulianDate
calendarToUT1 long (CalendarTime cday tod) = (fromIntegral (calendarToDay cday)) + (timeOfDayToDayFraction tod) - (long / 360)
