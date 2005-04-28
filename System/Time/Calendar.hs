{-# OPTIONS -ffi -Wall -Werror #-}

module System.Time.Calendar
(
	-- time zones
	Timezone,timezoneToMinutes,minutesToTimezone,hoursToTimezone,utc,

	-- getting the locale time zone
	getTimezone,getCurrentTimezone,

	-- TimeOfDay
	TimeOfDay(..),midnight,midday,

	-- Gregorian "calendrical" format
	CalendarDay(..),CalendarTime(..),
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
import Data.Fixed
import Data.Char

import Foreign
import Foreign.C

-- | count of minutes
newtype Timezone = MkTimezone {
	timezoneToMinutes :: Int
} deriving (Eq,Ord)

minutesToTimezone :: Int -> Timezone
minutesToTimezone = MkTimezone

hoursToTimezone :: Int -> Timezone
hoursToTimezone i = minutesToTimezone (60 * i)

instance Show Timezone where
	show (MkTimezone t) | t < 0 = '-':(show (MkTimezone (negate t)))
	show (MkTimezone t) = (show2 (div t 60)) ++ (show2 (mod t 60))

-- | The UTC time zone
utc :: Timezone
utc = minutesToTimezone 0

foreign import ccall unsafe "timestuff.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> IO CLong

posixToCTime :: POSIXTime -> CTime
posixToCTime  = fromInteger . floor

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimezone :: UTCTime -> IO Timezone
getTimezone time = do
	secs <- get_current_timezone_seconds (posixToCTime (utcTimeToPOSIXSeconds time))
	case secs of
		0x80000000 -> fail "localtime_r failed"
		_ -> return (minutesToTimezone (div (fromIntegral secs) 60))

-- | Get the current time-zone
getCurrentTimezone :: IO Timezone
getCurrentTimezone = getCurrentTime >>= getTimezone

-- | time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day
data TimeOfDay = TimeOfDay {
	todHour    :: Int,
	todMin     :: Int,
	todSec     :: Pico
} deriving (Eq,Ord)

midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

midday :: TimeOfDay
midday = TimeOfDay 12 0 0

show2 :: Int -> String
show2 i = let
	s = show i in
  case s of
	[_] -> '0':s
	_ -> s

show2Fixed :: Pico -> String
show2Fixed x | x < 10 = '0':(showFixed True x)
show2Fixed x = showFixed True x

instance Show TimeOfDay where
	show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

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
utcToLocalTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay (MkTimezone tz) (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
	m' = m + tz
	h' = h + (div m' 60)

-- | convert a ToD in some timezone to a ToD in UTC, together with a day adjustment
localToUTCTimeOfDay :: Timezone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay (MkTimezone tz) = utcToLocalTimeOfDay (MkTimezone (negate tz))

posixDay :: DiffTime
posixDay = fromInteger 86400

-- | get a TimeOfDay given a time since midnight
-- | time more than 24h will be converted to leap-seconds
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDay = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDay)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s where
	s' = realToFrac dt
	s = mod' s' 60
	m' = div' s' 60
	m = mod' m' 60
	h = div' m' 60

-- | find out how much time since midnight a given TimeOfDay is
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | show a UTC time in a given time zone as a CalendarTime
utcToCalendar :: Timezone -> UTCTime -> CalendarTime
utcToCalendar tz (UTCTime day dt) = CalendarTime (dayToCalendar (day + i)) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | find out what UTC time a given CalendarTime in a given time zone is
calendarToUTC :: Timezone -> CalendarTime -> UTCTime
calendarToUTC tz (CalendarTime cday tod) = UTCTime (day + i) (timeOfDayToTime todUTC) where
	day = calendarToDay cday
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | get a TimeOfDay given the fraction of a day since midnight
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | 1st arg is observation meridian in degrees, positive is East
ut1ToCalendar :: Rational -> ModJulianDate -> CalendarTime
ut1ToCalendar long date = CalendarTime (dayToCalendar localDay) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localDay = floor localTime
	localToDOffset = localTime - (fromIntegral localDay)

-- | get the fraction of a day since midnight given a TimeOfDay
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod / posixDay)
	
-- | 1st arg is observation meridian in degrees, positive is East
calendarToUT1 :: Rational -> CalendarTime -> ModJulianDate
calendarToUT1 long (CalendarTime cday tod) = (fromIntegral (calendarToDay cday)) + (timeOfDayToDayFraction tod) - (long / 360)
