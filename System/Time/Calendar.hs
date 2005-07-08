{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar
(
	module System.Time.Calendar.Timezone,
	module System.Time.Calendar.TimeOfDay,
	module System.Time.Calendar.Calendar,
	module System.Time.Calendar.YearDay,
	module System.Time.Calendar.Gregorian,
	module System.Time.Calendar.ISOWeekDay,
	module System.Time.Calendar.Format,
	-- * CalendarTime
	module System.Time.Calendar
) where

import Data.Fixed
import System.Time.Clock

import System.Time.Calendar.Timezone
import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Calendar
import System.Time.Calendar.YearDay
import System.Time.Calendar.Gregorian
import System.Time.Calendar.ISOWeekDay
import System.Time.Calendar.Format

type CalendarTime = ZonedTime (DayAndTime GregorianDay)

calendarTime :: Timezone -> Integer -> Int -> Int -> Int -> Int -> Pico -> CalendarTime
calendarTime zone year month day hour minute second = 
	ZonedTime (DayAndTime (GregorianDay year month day) (TimeOfDay hour minute second)) zone

ctZone :: CalendarTime -> Timezone
ctZone = ztZone

ctYear :: CalendarTime -> Integer
ctYear = gregYear . dtDay . ztTime

ctMonth :: CalendarTime -> Int
ctMonth = gregMonth . dtDay . ztTime

ctDay :: CalendarTime -> Int
ctDay = gregDay . dtDay . ztTime

ctHour :: CalendarTime -> Int
ctHour = todHour . dtTime . ztTime

ctMin :: CalendarTime -> Int
ctMin = todMin . dtTime . ztTime

ctSec :: CalendarTime -> Pico
ctSec = todSec . dtTime . ztTime

getCalendarTime :: IO CalendarTime
getCalendarTime = do
	t <- getCurrentTime
	zone <- getTimezone t
	return (encodeUTC zone t)
