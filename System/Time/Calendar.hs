module System.Time.Calendar
(
	-- time zones
	TimeZone,

	-- getting the locale time zone

	-- converting times to Gregorian "calendrical" format
	TimeOfDay,CalendarDay,CalendarTime

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"

	-- parsing and showing dates and times
) where

-- | count of minutes
newtype TimeZone = MkTimeZone Int deriving (Eq,Ord,Num)


data TimeOfDay = TimeOfDay
{
	todHour    :: Int,
	todMin     :: Int,
	todSec     :: Int,
	todPicosec :: Integer
} deriving (Eq,Ord)

instance Show TimeOfDay where
	show (TimeOfDay h m s ps) = 

data CalendarDay = CalendarDay
{
	cdYear    :: Integer,
	cdMonth   :: Int,
	cdDay     :: Int
} deriving (Eq,Ord)

data CalendarTime = CalendarTime
{
	ctDay    :: CalendarDay,
	ctTime   :: TimeOfDay
} deriving (Eq,Ord)



-- ((365 * 3 + 366) * 24 + 365 * 4) * 3 + (365 * 3 + 366) * 25
dayToCalendar :: ModJulianDay -> CalendarDay
dayToCalendar mjd = let
	a = mjd + 2000	-- ?
	quadcent = a / 146097
	b = a % 146097
	cent = min (b / 36524) 3
	...to be continued


utcToCalendar :: TimeZone -> UTCTime -> CalendarTime

calendarToUTC :: TimeZone -> CalendarTime -> UTCTime


