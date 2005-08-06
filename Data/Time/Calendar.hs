{-# OPTIONS -Wall -Werror #-}

module Data.Time.Calendar
(
	module Data.Time.Calendar.Days,
	module Data.Time.Calendar.YearDay,
	module Data.Time.Calendar.Gregorian,
	module Data.Time.Calendar.ISOWeekDay,
	module Data.Time.Calendar.Timezone,
	module Data.Time.Calendar.TimeOfDay,
	module Data.Time.Calendar.Calendar,
	module Data.Time.Calendar.Format,
	-- * CalendarTime
	module Data.Time.Calendar
) where

--import Data.Fixed
--import Data.Time.Clock

import Data.Time.Calendar.Days
import Data.Time.Calendar.YearDay
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.ISOWeekDay
import Data.Time.Calendar.Timezone
import Data.Time.Calendar.TimeOfDay
import Data.Time.Calendar.Calendar
import Data.Time.Calendar.Format

{-
type CalendarTime = ZonedTime (DayAndTime GregorianDay)

calendarTime :: Timezone -> Integer -> Int -> Int -> Int -> Int -> Pico -> CalendarTime
calendarTime zone year month day hour minute second = 
	ZonedTime (DayAndTime (GregorianDay year month day) (TimeOfDay hour minute second)) zone

ctZone :: CalendarTime -> Timezone
ctZone = ztZone

ctYear :: CalendarTime -> Integer
ctYear = gregYear . dtDay . ztLocalTime

ctMonth :: CalendarTime -> Int
ctMonth = gregMonth . dtDay . ztLocalTime

ctDay :: CalendarTime -> Int
ctDay = gregDay . dtDay . ztLocalTime

ctHour :: CalendarTime -> Int
ctHour = todHour . dtTime . ztLocalTime

ctMin :: CalendarTime -> Int
ctMin = todMin . dtTime . ztLocalTime

ctSec :: CalendarTime -> Pico
ctSec = todSec . dtTime . ztLocalTime
-}
