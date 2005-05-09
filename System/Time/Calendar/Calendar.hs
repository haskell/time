{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Calendar
(
	-- "Calendrical" format
	CalendarTime(..),DayEncoding(..),

	-- converting UTC and UT1 times to "calendrical" format
	LocalTimeEncoding(..),
	
	ZonedTime(..),encodeUTC,decodeUTC
) where

import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Clock

class (Eq d) => DayEncoding d where
	-- | name the given day according to the calendar
	encodeDay :: ModJulianDay -> d
	-- | find out which day a given calendar day is
	decodeDay :: d -> ModJulianDay

instance DayEncoding ModJulianDay where
	encodeDay = id
	decodeDay = id

class (Eq t) => LocalTimeEncoding t where
	-- | show a UTC time in a given time zone as a t
	encodeLocalUTC :: Timezone -> UTCTime -> t
	-- | find out what UTC time a given t in a given time zone is
	decodeLocalUTC :: Timezone -> t -> UTCTime
	-- | 1st arg is observation meridian in degrees, positive is East
	encodeLocalUT1 :: Rational -> ModJulianDate -> t
	-- | 1st arg is observation meridian in degrees, positive is East
	decodeLocalUT1 :: Rational -> t -> ModJulianDate

-- | straightforward date and time aggregate
data CalendarTime d = CalendarTime {
	ctDay    :: d,
	ctTime   :: TimeOfDay
} deriving (Eq,Ord)

instance (Show d) => Show (CalendarTime d) where
	show (CalendarTime d t) = (show d) ++ " " ++ (show t)

instance (DayEncoding d) => LocalTimeEncoding (CalendarTime d) where
	encodeLocalUTC tz (UTCTime day dt) = CalendarTime (encodeDay (day + i)) tod where
		(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)
	decodeLocalUTC tz (CalendarTime cday tod) = UTCTime (day + i) (timeOfDayToTime todUTC) where
		day = decodeDay cday
		(i,todUTC) = localToUTCTimeOfDay tz tod
	encodeLocalUT1 long date = CalendarTime (encodeDay localDay) (dayFractionToTimeOfDay localToDOffset) where
		localTime = date + long / 360 :: Rational
		localDay = floor localTime
		localToDOffset = localTime - (fromIntegral localDay)	
	decodeLocalUT1 long (CalendarTime cday tod) = (fromIntegral (decodeDay cday)) + (timeOfDayToDayFraction tod) - (long / 360)

data ZonedTime t = ZonedTime {
	ztTime :: t,
	ztZone :: Timezone
}

encodeUTC :: (LocalTimeEncoding t) => Timezone -> UTCTime -> ZonedTime t
encodeUTC zone time = ZonedTime (encodeLocalUTC zone time) zone

decodeUTC :: (LocalTimeEncoding t) => ZonedTime t -> UTCTime
decodeUTC (ZonedTime t zone) = decodeLocalUTC zone t

instance (Show t) => Show (ZonedTime t) where
	show (ZonedTime t zone) = show t ++ " " ++ show zone
