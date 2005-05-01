{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Calendar
(
	-- "Calendrical" format
	CalendarTime(..),Calendar(..),

	-- converting UTC times to "calendrical" format
	utcToCalendar,calendarToUTC,

	-- converting UT1 times to "calendrical" format
	ut1ToCalendar,calendarToUT1

	-- calendrical arithmetic
    -- e.g. "one month after March 31st"

	-- parsing and showing dates and times
) where

import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Calendar.Format
import System.Time.Clock
import System.Locale
import Data.Maybe

class (Show d) => Calendar d where
	-- | name the given day according to the calendar
	dayToCalendar :: ModJulianDay -> d
	-- | find out which day a given calendar day is
	calendarToMaybeDay :: d -> Maybe ModJulianDay
	calendarToDay :: d -> ModJulianDay
	calendarToDay day = fromMaybe (error "invalid day") (calendarToMaybeDay day)

-- | straightforward date and time aggregate
data CalendarTime calendarday = CalendarTime {
	ctDay    :: calendarday,
	ctTime   :: TimeOfDay
} deriving (Eq,Ord)

instance (Show calendarday) => Show (CalendarTime calendarday) where
	show (CalendarTime d t) = (show d) ++ " " ++ (show t)

melse :: Maybe a -> Maybe a -> Maybe a
melse ma@(Just _) _ = ma
melse _ mb = mb

instance (FormatTime d) => FormatTime (CalendarTime d) where
	formatCharacter locale 'c' dt = Just (formatTime locale (dateTimeFmt locale) dt)
	formatCharacter locale c (CalendarTime d t) =
		melse (formatCharacter locale c d) (formatCharacter locale c t)

-- | show a UTC time in a given time zone as a CalendarTime
utcToCalendar :: (Calendar d) => Timezone -> UTCTime -> CalendarTime d
utcToCalendar tz (UTCTime day dt) = CalendarTime (dayToCalendar (day + i)) tod where
	(i,tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)

-- | find out what UTC time a given CalendarTime in a given time zone is
calendarToUTC :: (Calendar d) => Timezone -> CalendarTime d -> UTCTime
calendarToUTC tz (CalendarTime cday tod) = UTCTime (day + i) (timeOfDayToTime todUTC) where
	day = calendarToDay cday
	(i,todUTC) = localToUTCTimeOfDay tz tod

-- | 1st arg is observation meridian in degrees, positive is East
ut1ToCalendar :: (Calendar day) => Rational -> ModJulianDate -> CalendarTime day
ut1ToCalendar long date = CalendarTime (dayToCalendar localDay) (dayFractionToTimeOfDay localToDOffset) where
	localTime = date + long / 360 :: Rational
	localDay = floor localTime
	localToDOffset = localTime - (fromIntegral localDay)
	
-- | 1st arg is observation meridian in degrees, positive is East
calendarToUT1 :: (Calendar day) => Rational -> CalendarTime day -> ModJulianDate
calendarToUT1 long (CalendarTime cday tod) = (fromIntegral (calendarToDay cday)) + (timeOfDayToDayFraction tod) - (long / 360)
