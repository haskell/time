{-# LANGUAGE Safe #-}

module Data.Time.LocalTime (
    -- * Time zones
    TimeZone (..),
    timeZoneOffsetString,
    timeZoneOffsetString',
    minutesToTimeZone,
    hoursToTimeZone,
    utc,
    -- getting the locale time zone
    getTimeZone,
    getCurrentTimeZone,

    -- * Time of day
    TimeOfDay (..),
    midnight,
    midday,
    makeTimeOfDayValid,
    timeToDaysAndTimeOfDay,
    daysAndTimeOfDayToTime,
    utcToLocalTimeOfDay,
    localToUTCTimeOfDay,
    timeToTimeOfDay,
    pastMidnight,
    timeOfDayToTime,
    sinceMidnight,
    diffTimeOfDay,
    dayFractionToTimeOfDay,
    timeOfDayToDayFraction,

    -- * Calendar Duration
    module Data.Time.LocalTime.Internal.CalendarDiffTime,

    -- * Local Time
    LocalTime (..),
    addLocalTime,
    diffLocalTime,
    -- converting UTC and UT1 times to LocalTime
    utcToLocalTime,
    localTimeToUTC,
    ut1ToLocalTime,
    localTimeToUT1,
    -- using CalendarDiffTime
    addLocalDurationClip,
    addLocalDurationRollOver,
    diffLocalDurationClip,
    diffLocalDurationRollOver,

    -- * Zoned Time
    ZonedTime (..),
    utcToZonedTime,
    zonedTimeToUTC,
    getZonedTime,
    utcToLocalZonedTime,
) where

import Data.Time.Format ()
import Data.Time.LocalTime.Internal.CalendarDiffTime
import Data.Time.LocalTime.Internal.Foreign
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone hiding (timeZoneOffsetString'')
import Data.Time.LocalTime.Internal.ZonedTime
