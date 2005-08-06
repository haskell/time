{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.LocalTime
(
	-- * Local Time
	LocalTime(..),

	-- converting UTC and UT1 times to LocalTime
	utcToLocalTime,localTimeToUTC,ut1ToLocalTime,localTimeToUT1,
	
	ZonedTime(..),zonedTimeFromUTC,ztUTC,getZonedTime
) where

import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.Timezone
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Days
import Data.Time.Clock

-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
data LocalTime = LocalTime {
	localDay    :: Date,
	localTimeOfDay   :: TimeOfDay
} deriving (Eq,Ord)

instance Show LocalTime where
	show (LocalTime d t) = (showGregorian d) ++ " " ++ (show t)

-- | show a UTC time in a given time zone as a LocalTime
utcToLocalTime :: Timezone -> UTCTime -> LocalTime
utcToLocalTime tz (UTCTime day dt) = LocalTime (addDate day i) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | find out what UTC time a given LocalTime in a given time zone is
localTimeToUTC :: Timezone -> LocalTime -> UTCTime
localTimeToUTC tz (LocalTime day tod) = UTCTime (addDate day i) (timeOfDayToTime todUTC) where
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | 1st arg is observation meridian in degrees, positive is East
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime long (ModJulianDate date) = LocalTime (ModJulianDay localMJD) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localMJD = floor localTime
	localToDOffset = localTime - (fromIntegral localMJD)	

-- | 1st arg is observation meridian in degrees, positive is East
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 long (LocalTime (ModJulianDay localMJD) tod) = ModJulianDate ((fromIntegral localMJD) + (timeOfDayToDayFraction tod) - (long / 360))

-- | A local time together with a Timezone.
data ZonedTime = ZonedTime {
	ztLocalTime :: LocalTime,
	ztZone :: Timezone
}

zonedTimeFromUTC :: Timezone -> UTCTime -> ZonedTime
zonedTimeFromUTC zone time = ZonedTime (utcToLocalTime zone time) zone

ztUTC :: ZonedTime -> UTCTime
ztUTC (ZonedTime t zone) = localTimeToUTC zone t

instance Show ZonedTime where
	show (ZonedTime t zone) = show t ++ " " ++ show zone

getZonedTime :: IO ZonedTime
getZonedTime = do
	t <- getCurrentTime
	zone <- getTimezone t
	return (zonedTimeFromUTC zone t)
