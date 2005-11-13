{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.LocalTime
(
	-- * Local Time
	LocalTime(..),

	-- converting UTC and UT1 times to LocalTime
	utcToLocalTime,localTimeToUTC,ut1ToLocalTime,localTimeToUT1,
	
	ZonedTime(..),utcToZonedTime,zonedTimeToUTC,getZonedTime,utcToLocalZonedTime
) where

import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone
import Data.Time.Calendar
import Data.Time.Clock

-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
data LocalTime = LocalTime {
	localDay    :: Day,
	localTimeOfDay   :: TimeOfDay
} deriving (Eq,Ord)

instance Show LocalTime where
	show (LocalTime d t) = (showGregorian d) ++ " " ++ (show t)

-- | show a UTC time in a given time zone as a LocalTime
utcToLocalTime :: TimeZone -> UTCTime -> LocalTime
utcToLocalTime tz (UTCTime day dt) = LocalTime (addDays i day) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | find out what UTC time a given LocalTime in a given time zone is
localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
localTimeToUTC tz (LocalTime day tod) = UTCTime (addDays i day) (timeOfDayToTime todUTC) where
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | 1st arg is observation meridian in degrees, positive is East
ut1ToLocalTime :: Rational -> UniversalTime -> LocalTime
ut1ToLocalTime long (ModJulianDate date) = LocalTime (ModifiedJulianDay localMJD) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localMJD = floor localTime
	localToDOffset = localTime - (fromIntegral localMJD)	

-- | 1st arg is observation meridian in degrees, positive is East
localTimeToUT1 :: Rational -> LocalTime -> UniversalTime
localTimeToUT1 long (LocalTime (ModifiedJulianDay localMJD) tod) = ModJulianDate ((fromIntegral localMJD) + (timeOfDayToDayFraction tod) - (long / 360))

-- | A local time together with a TimeZone.
data ZonedTime = ZonedTime {
	zonedTimeToLocalTime :: LocalTime,
	zonedTimeZone :: TimeZone
}

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime zone time = ZonedTime (utcToLocalTime zone time) zone

zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC (ZonedTime t zone) = localTimeToUTC zone t

instance Show ZonedTime where
	show (ZonedTime t zone) = show t ++ " " ++ show zone

instance Show UTCTime where
	show t = show (utcToZonedTime utc t)

getZonedTime :: IO ZonedTime
getZonedTime = do
	t <- getCurrentTime
	zone <- getTimeZone t
	return (utcToZonedTime zone t)

-- |
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime t = do
	zone <- getTimeZone t
	return (utcToZonedTime zone t)
