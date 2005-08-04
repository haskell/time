{-# OPTIONS -Wall -Werror #-}


-- #hide
module System.Time.Calendar.Format
	(
	-- * UNIX-style formatting
	module System.Time.Calendar.Format
	) where

import System.Time.Calendar.ISOWeekDay
import System.Time.Calendar.Gregorian
import System.Time.Calendar.YearDay
import System.Time.Calendar.Days
import System.Time.Calendar.Calendar
import System.Time.Calendar.TimeOfDay
import System.Time.Calendar.Timezone
import System.Time.Calendar.Private
import System.Time.Clock
import System.Time.Clock.POSIX

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

instance FormatTime DayAndTime where
	formatCharacter 'c' = Just (\locale -> formatTime locale (dateTimeFmt locale))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale dt -> f locale (dtDay dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale dt -> f locale (dtTime dt))
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

instance FormatTime ZonedTime where
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

instance FormatTime Date where
	-- Aggregate
	formatCharacter 'D' = Just (\locale -> formatTime locale "%m/%d/%y")
	formatCharacter 'F' = Just (\locale -> formatTime locale "%Y-%m-%d")
	formatCharacter 'x' = Just (\locale -> formatTime locale (dateFmt locale))

	-- Year Count
	formatCharacter 'Y' = Just (\_ -> show . fst . yearAndDay)
	formatCharacter 'y' = Just (\_ -> show2 . fromInteger . mod100 . fst . yearAndDay)
	formatCharacter 'C' = Just (\_ -> show2 . fromInteger . div100 . fst . yearAndDay)
	-- Month of Year
	formatCharacter 'B' = Just (\locale -> fst . (\(_,m,_) -> (months locale) !! (m - 1)) . gregorian)
	formatCharacter 'b' = Just (\locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . gregorian)
	formatCharacter 'h' = Just (\locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . gregorian)
	formatCharacter 'm' = Just (\_ -> show2 . (\(_,m,_) -> m) . gregorian)
	-- Day of Month
	formatCharacter 'd' = Just (\_ -> show2 . (\(_,_,d) -> d) . gregorian)
	formatCharacter 'e' = Just (\_ -> show2Space . (\(_,_,d) -> d) . gregorian)
	-- Day of Year
	formatCharacter 'j' = Just (\_ -> show3 . snd . yearAndDay)

	-- ISOWeekDay
	formatCharacter 'G' = Just (\_ -> show . (\(y,_,_) -> y) . isoWeekDay)
	formatCharacter 'g' = Just (\_ -> show2 . fromInteger . mod100 . (\(y,_,_) -> y) . isoWeekDay)
	formatCharacter 'V' = Just (\_ -> show2 . (\(_,w,_) -> w) . isoWeekDay)
	formatCharacter 'u' = Just (\_ -> show . (\(_,_,d) -> d) . isoWeekDay)

	-- Day of week
	formatCharacter 'a' = Just (\locale -> snd . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'A' = Just (\locale -> fst . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'U' = Just (\_ -> show2 . fst . sundayStartWeek)
	formatCharacter 'w' = Just (\_ -> show . snd . sundayStartWeek)
	formatCharacter 'W' = Just (\_ -> show2 . fst . mondayStartWeek)
	
	-- Default
	formatCharacter _   = Nothing

instance FormatTime UTCTime where
	formatCharacter c = fmap (\f locale t -> f locale (encodeUTC utc t)) (formatCharacter c)