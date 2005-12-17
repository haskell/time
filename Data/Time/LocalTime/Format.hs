{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.Format
	(
	-- * UNIX-style formatting
	module Data.Time.LocalTime.Format
	) where

import Data.Time.LocalTime.LocalTime
import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Time.Clock.POSIX

import System.Locale
import Data.Maybe
import Data.Char

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
	formatCharacter :: Char -> Maybe (TimeLocale -> t -> String)

-- | Substitute various time-related information for each %-code in the string, as per 'formatCharacter'.
--
-- For all types (note these three are done here, not by 'formatCharacter'):
--
-- [@%%@] @%@
--
-- [@%t@] tab
--
-- [@%n@] newline
--
-- For TimeZone (and ZonedTime and UTCTime):
--
-- [@%z@] timezone offset
--
-- [@%Z@] timezone name
--
-- For LocalTime (and ZonedTime and UTCTime):
--
-- [@%c@] as 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@)
--
-- For TimeOfDay (and LocalTime and ZonedTime and UTCTime):
--
-- [@%R@] same as @%H:%M@
--
-- [@%T@] same as @%H:%M:%S@
--
-- [@%X@] as 'timeFmt' @locale@ (e.g. @%H:%M:%S@)
--
-- [@%r@] as 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@)
--
-- [@%P@] day half from ('amPm' @locale@), converted to lowercase, @am@, @pm@
--
-- [@%p@] day half from ('amPm' @locale@), @AM@, @PM@
--
-- [@%H@] hour, 24-hour, leading 0 as needed, @00@ - @23@
--
-- [@%I@] hour, 12-hour, leading 0 as needed, @01@ - @12@
--
-- [@%k@] hour, 24-hour, leading space as needed, @ 0@ - @23@
--
-- [@%l@] hour, 12-hour, leading space as needed, @ 1@ - @12@
--
-- [@%M@] minute, @00@ - @59@
--
-- [@%S@] second with decimal part if not an integer, @00@ - @60.999999999999@
--
-- For UTCTime and ZonedTime:
--
-- [@%s@] number of seconds since the Unix epoch
--
-- For Day (and LocalTime and ZonedTime and UTCTime):
--
-- [@%D@] same as @%m\/%d\/%y@
--
-- [@%F@] same as @%Y-%m-%d@
--
-- [@%x@] as 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@)
--
-- [@%Y@] year
--
-- [@%y@] last two digits of year, @00@ - @99@
--
-- [@%C@] century (being the first two digits of the year), @00@ - @99@
--
-- [@%B@] month name, long form ('fst' from 'months' @locale@), @January@ - @December@
--
-- [@%b@, @%h@] month name, short form ('snd' from 'months' @locale@), @Jan@ - @Dec@
--
-- [@%m@] month of year, leading 0 as needed, @01@ - @12@
--
-- [@%d@] day of month, leading 0 as needed, @01@ - @31@
--
-- [@%e@] day of month, leading space as needed,  @ 1@ - @31@
--
-- [@%j@] day of year for Ordinal Date format, @001@ - @366@
--
-- [@%G@] year for Week Date format
--
-- [@%g@] last two digits of year for Week Date format, @00@ - @99@
--
-- [@%V@] week for Week Date format, @01@ - @53@
--
-- [@%u@] day for Week Date format, @1@ - @7@
--
-- [@%a@] day of week, short form ('snd' from 'wDays' @locale@), @Sun@ - @Sat@
--
-- [@%A@] day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ - @Saturday@
--
-- [@%U@] week number of year, where weeks start on Sunday (as 'sundayStartWeek'), @01@ - @53@
--
-- [@%w@] day of week number, @0@ (= Sunday) - @6@ (= Saturday)
--
-- [@%W@] week number of year, where weeks start on Monday (as 'mondayStartWeek'), @01@ - @53@
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

instance FormatTime LocalTime where
	formatCharacter 'c' = Just (\locale -> formatTime locale (dateTimeFmt locale))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale dt -> f locale (localDay dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale dt -> f locale (localTimeOfDay dt))
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
	formatCharacter 's' = Just (\_ zt -> show (truncate (utcTimeToPOSIXSeconds (zonedTimeToUTC zt)) :: Integer))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale dt -> f locale (zonedTimeToLocalTime dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale dt -> f locale (zonedTimeZone dt))
			Nothing -> Nothing

instance FormatTime TimeZone where
	formatCharacter 'z' = Just (\_ -> timeZoneOffsetString)
	formatCharacter 'Z' = Just (\_ -> timeZoneName)
	formatCharacter _ = Nothing

instance FormatTime Day where
	-- Aggregate
	formatCharacter 'D' = Just (\locale -> formatTime locale "%m/%d/%y")
	formatCharacter 'F' = Just (\locale -> formatTime locale "%Y-%m-%d")
	formatCharacter 'x' = Just (\locale -> formatTime locale (dateFmt locale))

	-- Year Count
	formatCharacter 'Y' = Just (\_ -> show . fst . toOrdinalDate)
	formatCharacter 'y' = Just (\_ -> show2 . mod100 . fst . toOrdinalDate)
	formatCharacter 'C' = Just (\_ -> show2 . div100 . fst . toOrdinalDate)
	-- Month of Year
	formatCharacter 'B' = Just (\locale -> fst . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'b' = Just (\locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'h' = Just (\locale -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'm' = Just (\_ -> show2 . (\(_,m,_) -> m) . toGregorian)
	-- Day of Month
	formatCharacter 'd' = Just (\_ -> show2 . (\(_,_,d) -> d) . toGregorian)
	formatCharacter 'e' = Just (\_ -> show2Space . (\(_,_,d) -> d) . toGregorian)
	-- Day of Year
	formatCharacter 'j' = Just (\_ -> show3 . snd . toOrdinalDate)

	-- ISO 8601 Week Date
	formatCharacter 'G' = Just (\_ -> show . (\(y,_,_) -> y) . toWeekDate)
	formatCharacter 'g' = Just (\_ -> show2 . mod100 . (\(y,_,_) -> y) . toWeekDate)
	formatCharacter 'V' = Just (\_ -> show2 . (\(_,w,_) -> w) . toWeekDate)
	formatCharacter 'u' = Just (\_ -> show . (\(_,_,d) -> d) . toWeekDate)

	-- Day of week
	formatCharacter 'a' = Just (\locale -> snd . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'A' = Just (\locale -> fst . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'U' = Just (\_ -> show2 . fst . sundayStartWeek)
	formatCharacter 'w' = Just (\_ -> show . snd . sundayStartWeek)
	formatCharacter 'W' = Just (\_ -> show2 . fst . mondayStartWeek)
	
	-- Default
	formatCharacter _   = Nothing

instance FormatTime UTCTime where
	formatCharacter c = fmap (\f locale t -> f locale (utcToZonedTime utc t)) (formatCharacter c)
