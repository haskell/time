{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.Calendar
(
	-- * Classes
	-- "Calendrical" format
	DayAndTime(..),DayEncoding(..),

	-- converting UTC and UT1 times to "calendrical" format
	LocalTimeEncoding(..),
	
	ZonedTime(..),encodeUTC,decodeUTC
) where

import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Clock

-- | A type that encodes a day number.
class (Eq d) => DayEncoding d where
	-- | Name the given day according to the calendar.
	encodeDay :: ModJulianDay -> d
	-- | Find out which day a given calendar day is.
	-- Behaviour for invalid representations is not defined.
	decodeDay :: d -> ModJulianDay

instance DayEncoding ModJulianDay where
	encodeDay = id
	decodeDay = id

-- | A type that encodes a local representation of a time, either local civil time or local mean time.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
class (Eq t) => LocalTimeEncoding t where
	-- | show a UTC time in a given time zone as a t
	encodeLocalUTC :: Timezone -> UTCTime -> t
	-- | find out what UTC time a given t in a given time zone is
	decodeLocalUTC :: Timezone -> t -> UTCTime
	-- | 1st arg is observation meridian in degrees, positive is East
	encodeLocalUT1 :: Rational -> ModJulianDate -> t
	-- | 1st arg is observation meridian in degrees, positive is East
	decodeLocalUT1 :: Rational -> t -> ModJulianDate

-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
data DayAndTime d = DayAndTime {
	dtDay    :: d,
	dtTime   :: TimeOfDay
} deriving (Eq,Ord)

instance (Show d) => Show (DayAndTime d) where
	show (DayAndTime d t) = (show d) ++ " " ++ (show t)

instance (DayEncoding d) => LocalTimeEncoding (DayAndTime d) where
	encodeLocalUTC tz (UTCTime day dt) = DayAndTime (encodeDay (day + i)) tod where
		(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)
	decodeLocalUTC tz (DayAndTime cday tod) = UTCTime (day + i) (timeOfDayToTime todUTC) where
		day = decodeDay cday
		(i,todUTC) = localToUTCTimeOfDay tz tod
	encodeLocalUT1 long date = DayAndTime (encodeDay localDay) (dayFractionToTimeOfDay localToDOffset) where
		localTime = date + long / 360 :: Rational
		localDay = floor localTime
		localToDOffset = localTime - (fromIntegral localDay)	
	decodeLocalUT1 long (DayAndTime cday tod) = (fromIntegral (decodeDay cday)) + (timeOfDayToDayFraction tod) - (long / 360)

-- | A local time together with a Timezone.
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
