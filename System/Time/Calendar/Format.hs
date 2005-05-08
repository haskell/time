{-# OPTIONS -Wall -Werror #-}

module System.Time.Calendar.Format where

import System.Time.Calendar.ISOWeek
import System.Time.Calendar.Gregorian
import System.Time.Calendar.YearDay
import System.Time.Calendar.Calendar
import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Calendar.Private
import System.Time.Clock

import System.Locale
import Data.Maybe
import Data.Char

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
	formatCharacter :: Char -> Maybe (TimeLocale -> t -> String)

formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime _ [] _ = ""
formatTime locale ('%':c:cs) t = (formatChar c) ++ (formatTime locale cs t) where
	formatChar '%' = "%"
	formatChar 't' = "\t"
	formatChar 'n' = "\n"
	formatChar _ = case (formatCharacter c) of
		Just f -> f locale t
		_ -> ""
formatTime locale (c:cs) t = c:(formatTime locale cs t)

instance (FormatTime d) => FormatTime (CalendarTime d) where
	formatCharacter 'c' = Just (\locale -> formatTime locale (dateTimeFmt locale))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale dt -> f locale (ctDay dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale dt -> f locale (ctTime dt))
			Nothing -> Nothing

instance FormatTime TimeOfDay where
	-- Aggregate
	formatCharacter 'R' = Just (\locale -> formatTime locale "%H:%M")
	formatCharacter 'T' = Just (\locale -> formatTime locale "%H:%M:%S")
	formatCharacter 'X' = Just (\locale -> formatTime locale (timeFmt locale))
	formatCharacter 'r' = Just (\locale -> formatTime locale (time12Fmt locale))
	-- AM/PM
	formatCharacter 'P' = Just (\locale day -> map toLower ((if (todHour day) < 12 then fst else snd) (amPm locale)))
	formatCharacter 'p' = Just (\locale day -> (if (todHour day) < 12 then fst else snd) (amPm locale))
	-- Hour
	formatCharacter 'H' = Just (\_ -> show2 . todHour)
	formatCharacter 'I' = Just (\_ -> show2 . (\h -> (mod (h - 1) 12) + 1) . todHour)
	formatCharacter 'k' = Just (\_ -> show2Space . todHour)
	formatCharacter 'l' = Just (\_ -> show2Space . (\h -> (mod (h - 1) 12) + 1) . todHour)
	-- Minute
	formatCharacter 'M' = Just (\_ -> show2 . todMin)
	-- Second
	formatCharacter 'S' = Just (\_ -> show2Fixed . todSec)

	-- Default
	formatCharacter _   = Nothing

instance (FormatTime t,LocalTimeEncoding t) => FormatTime (ZonedTime t) where
	formatCharacter 's' = Just (\_ zt -> show (truncate (utcTimeToPOSIXSeconds (decodeUTC zt)) :: Integer))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale dt -> f locale (ztTime dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale dt -> f locale (ztZone dt))
			Nothing -> Nothing

instance FormatTime Timezone where
	formatCharacter 'z' = Just (\_ -> timezoneOffsetString)
	formatCharacter 'Z' = Just (\_ -> timezoneName)
	formatCharacter _ = Nothing

weekDay :: ModJulianDay -> Int
weekDay day = fromInteger (mod (day + 3) 7)

weekDay' :: ModJulianDay -> Int
weekDay' day = weekDay (day - 1) + 1

dayOfYear :: ModJulianDay -> Int
dayOfYear = ydDay . encodeDay

weekNumber :: ModJulianDay -> Int
weekNumber day = (div (dayOfYear day) 7) + 1

weekNumber' :: ModJulianDay -> Int
weekNumber' day = (div (dayOfYear day) 7) + 1

instance FormatTime ModJulianDay where
	-- Aggregate
	formatCharacter 'D' = Just (\locale -> formatTime locale "%m/%d/%y")
	formatCharacter 'F' = Just (\locale -> formatTime locale "%Y-%m-%d")
	formatCharacter 'x' = Just (\locale -> formatTime locale (dateFmt locale))

	-- Year Count
	formatCharacter 'Y' = Just (\_ -> show . cdYear . encodeDay)
	formatCharacter 'y' = Just (\_ -> show2 . fromInteger . mod100 . cdYear . encodeDay)
	formatCharacter 'C' = Just (\_ -> show2 . fromInteger . div100 . cdYear . encodeDay)
	-- Month of Year
	formatCharacter 'B' = Just (\locale -> fst . (\m -> (months locale) !! (m - 1)) . cdMonth . encodeDay)
	formatCharacter 'b' = Just (\locale -> snd . (\m -> (months locale) !! (m - 1)) . cdMonth . encodeDay)
	formatCharacter 'h' = Just (\locale -> snd . (\m -> (months locale) !! (m - 1)) . cdMonth . encodeDay)
	formatCharacter 'm' = Just (\_ -> show2 . cdMonth . encodeDay)
	-- Day of Month
	formatCharacter 'd' = Just (\_ -> show2 . cdDay . encodeDay)
	formatCharacter 'e' = Just (\_ -> show2Space . cdDay . encodeDay)
	-- Day of Year
	formatCharacter 'j' = Just (\_ -> show3 . ydDay . encodeDay)

	-- ISOWeek
	formatCharacter 'G' = Just (\_ -> show . isowYear . encodeDay)
	formatCharacter 'g' = Just (\_ -> show2 . fromInteger . mod100 . isowYear . encodeDay)
	formatCharacter 'V' = Just (\_ -> show2 . isowWeek . encodeDay)
	formatCharacter 'u' = Just (\_ -> show . isowDay . encodeDay)

	-- Day of week
	formatCharacter 'a' = Just (\locale -> snd . ((wDays locale) !!) . weekDay)
	formatCharacter 'A' = Just (\locale -> fst . ((wDays locale) !!) . weekDay)
	formatCharacter 'U' = Just (\_ -> show2 . weekNumber)
	formatCharacter 'w' = Just (\_ -> show . weekDay)
	formatCharacter 'W' = Just (\_ -> show2 . weekNumber')
	
	-- Default
	formatCharacter _   = Nothing

formatDayCharacter :: (DayEncoding d) => Char -> Maybe (TimeLocale -> d -> String)
formatDayCharacter c = do
	f <- formatCharacter c
	return (\locale d -> f locale (decodeDay d))

instance FormatTime YearDay where
	formatCharacter = formatDayCharacter

instance FormatTime GregorianDay where
	formatCharacter = formatDayCharacter

instance FormatTime ISOWeek where
	formatCharacter = formatDayCharacter
