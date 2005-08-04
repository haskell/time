{-# OPTIONS -Wall -Werror #-}

-- #hide
module System.Time.Calendar.Calendar
(
	-- * Classes
	-- "Calendrical" format
	DayAndTime(..),

	-- converting UTC and UT1 times to "calendrical" format
	encodeLocalUTC,decodeLocalUTC,encodeLocalUT1,decodeLocalUT1,
	
	ZonedTime(..),encodeUTC,decodeUTC,getZonedTime
) where

import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Calendar.Gregorian
import System.Time.Calendar.Days
import System.Time.Clock

-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
data DayAndTime = DayAndTime {
	dtDay    :: Date,
	dtTime   :: TimeOfDay
} deriving (Eq,Ord)

instance Show DayAndTime where
	show (DayAndTime d t) = (showGregorian d) ++ " " ++ (show t)

-- | show a UTC time in a given time zone as a DayAndTime
encodeLocalUTC :: Timezone -> UTCTime -> DayAndTime
encodeLocalUTC tz (UTCTime day dt) = DayAndTime (addDate day i) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | find out what UTC time a given DayAndTime in a given time zone is
decodeLocalUTC :: Timezone -> DayAndTime -> UTCTime
decodeLocalUTC tz (DayAndTime day tod) = UTCTime (addDate day i) (timeOfDayToTime todUTC) where
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | 1st arg is observation meridian in degrees, positive is East
encodeLocalUT1 :: Rational -> UniversalTime -> DayAndTime
encodeLocalUT1 long (ModJulianDate date) = DayAndTime (ModJulianDay localDay) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localDay = floor localTime
	localToDOffset = localTime - (fromIntegral localDay)	

-- | 1st arg is observation meridian in degrees, positive is East
decodeLocalUT1 :: Rational -> DayAndTime -> UniversalTime
decodeLocalUT1 long (DayAndTime (ModJulianDay localDay) tod) = ModJulianDate ((fromIntegral localDay) + (timeOfDayToDayFraction tod) - (long / 360))

-- | A local time together with a Timezone.
data ZonedTime = ZonedTime {
	ztTime :: DayAndTime,
	ztZone :: Timezone
}

encodeUTC :: Timezone -> UTCTime -> ZonedTime
encodeUTC zone time = ZonedTime (encodeLocalUTC zone time) zone

decodeUTC :: ZonedTime -> UTCTime
decodeUTC (ZonedTime t zone) = decodeLocalUTC zone t

instance Show ZonedTime where
	show (ZonedTime t zone) = show t ++ " " ++ show zone

getZonedTime :: IO ZonedTime
getZonedTime = do
	t <- getCurrentTime
	zone <- getTimezone t
	return (encodeUTC zone t)
