{-# OPTIONS -fno-warn-orphans -fno-warn-unused-imports #-}

-- #hide
module Data.Time.LocalTime.LocalTime
    ( -- * Local Time
      LocalTime(..)
      -- converting UTC and UT1 times to LocalTime
    , posixToZonedTime
    , utcToLocalTime
    , localTimeToUTC
    , ut1ToLocalTime
    , localTimeToUT1
    , ZonedTime(..)
    , utcToZonedTime
    , zonedTimeToUTC
    , getZonedTime
    , utcToLocalZonedTime
    ) where

import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian

import Data.Time.Clock.Scale
import Data.Time.Clock.UTCDiff
import Data.Time.Clock.UTC
import Data.Time.Clock.POSIX

import Data.Fixed

import Control.DeepSeq
import Data.Typeable
import Data.Data

-- | A simple day and time aggregate, where the day is of the specified parameter,
-- and the time is a TimeOfDay.
-- Conversion of this (as local civil time) to UTC depends on the time zone.
-- Conversion of this (as local mean time) to UT1 depends on the longitude.
data LocalTime = LocalTime {
    localDay        :: !Day,
    localTimeOfDay  :: !TimeOfDay
} deriving (Eq, Ord, Data, Typeable)

instance NFData LocalTime where
    rnf (LocalTime d t) = d `deepseq` t `deepseq` ()

instance Show LocalTime where
    show (LocalTime d t) = (showGregorian d) ++ " " ++ (show t)

-- | show a 'POSIXTime' in a given time zone as a LocalTime
posixToLocalTime :: TimeZone -> POSIXTime -> LocalTime
posixToLocalTime (TimeZone minz _ _) raw =
    LocalTime (fromIntegral day `addDays` unixEpochDay) tod
  where
    (POSIXTime s ns) = normalizePosix raw
    (day, s') = (s + (fromIntegral minz) * 60) `divMod` posixDayLength
    (m, ss) = s' `divMod` 60
    (hh, mm) = m `divMod` 60
    tod = TimeOfDay (fromIntegral hh)
                    (fromIntegral mm)
                    (MkFixed (fromIntegral (ss * 1000000000000 + ns * 1000)))

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

-- orphan instance
instance Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)

-- | A local time together with a TimeZone.
data ZonedTime = ZonedTime {
    zonedTimeToLocalTime :: !LocalTime,
    zonedTimeZone :: !TimeZone
} deriving (Data, Typeable)

instance NFData ZonedTime where
    rnf (ZonedTime lt z) = lt `deepseq` z `deepseq` ()

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime zone time = ZonedTime (utcToLocalTime zone time) zone

posixToZonedTime :: TimeZone -> POSIXTime -> ZonedTime
posixToZonedTime zone time = ZonedTime (posixToLocalTime zone time) zone

zonedTimeToUTC :: ZonedTime -> UTCTime
zonedTimeToUTC (ZonedTime t zone) = localTimeToUTC zone t

instance Show ZonedTime where
    show (ZonedTime t zone) = show t ++ " " ++ show zone

-- orphan instance
instance Show UTCTime where
    show t = show (utcToZonedTime utc t)

-- | Get 'ZonedTime' in local 'TimeZone'
--
-- Note this function and 'utcToLocalZonedTime' are quite slow,
-- because reading system's time zone is slow on some systems.
-- If your application can assume a stable system 'TimeZone',
-- then cache 'getTimeZone' 's result and use 'posixToZonedTime' or
-- 'utcToZonedTime' will be much faster(especially 'posixToZonedTime').
--
getZonedTime :: IO ZonedTime
getZonedTime = do
    raw <- getPOSIXTime
    zone <- getTimeZonePosix raw
    return $! posixToZonedTime zone raw

-- | convert 'UTCTime' to 'ZonedTime' using local 'TimeZone'.
utcToLocalZonedTime :: UTCTime -> IO ZonedTime
utcToLocalZonedTime t = do
    zone <- getTimeZone t
    return $! utcToZonedTime zone t
