{-# LANGUAGE Safe #-}

-- | Week-based calendars
module Data.Time.Calendar.WeekDate
    (
        Year, WeekOfYear, DayOfWeek(..), dayOfWeek,
        FirstWeekType (..),toWeekCalendar,fromWeekCalendar,fromWeekCalendarValid,
        -- * ISO 8601 Week Date format
        toWeekDate, fromWeekDate, pattern YearWeekDay,
        fromWeekDateValid, showWeekDate
    ) where

import Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Calendar.Week
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private


data FirstWeekType
    = FirstWholeWeek
    -- ^ first week is the first whole week of the year
    | FirstMostWeek
    -- ^ first week is the first week with four days in the year
    deriving Eq

firstDayOfWeekCalendar :: FirstWeekType -> DayOfWeek -> Year -> Day
firstDayOfWeekCalendar wt dow year = let
    jan1st = fromOrdinalDate year 1
    in case wt of
        FirstWholeWeek -> firstDayOfWeekOnAfter dow jan1st
        FirstMostWeek -> firstDayOfWeekOnAfter dow $ addDays (-3) jan1st

-- | Convert to the given kind of "week calendar".
-- Note that the year number matches the weeks, and so is not always the same as the Gregorian year number.
toWeekCalendar ::
    FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Day
    -> (Year, WeekOfYear, DayOfWeek)
toWeekCalendar wt ws d = let
    dw = dayOfWeek d
    (y0,_) = toOrdinalDate d
    j1p = firstDayOfWeekCalendar wt ws $ pred y0
    j1 = firstDayOfWeekCalendar wt ws y0
    j1s = firstDayOfWeekCalendar wt ws $ succ y0
    in if d < j1
        then (pred y0,succ $ div (fromInteger $ diffDays d j1p) 7,dw)
        else if d < j1s then (y0,succ $ div (fromInteger $ diffDays d j1) 7,dw)
        else (succ y0,succ $ div (fromInteger $ diffDays d j1s) 7,dw)

-- | Convert from the given kind of "week calendar".
-- Invalid week and day values will be clipped to the correct range.
fromWeekCalendar ::
    FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekCalendar wt ws y wy dw = let
    d1 :: Day
    d1 = firstDayOfWeekCalendar wt ws y
    wy' = clip 1 53 wy
    getday :: WeekOfYear -> Day
    getday wy'' = addDays (toInteger $ (pred wy'' * 7) + (dayOfWeekDiff dw ws)) d1
    d1s = firstDayOfWeekCalendar wt ws $ succ y
    day = getday wy'
    in if wy' == 53 then if day >= d1s then getday 52 else day else day

-- | Convert from the given kind of "week calendar".
-- Invalid week and day values will return Nothing.
fromWeekCalendarValid ::
     FirstWeekType
    -- ^ how to reckon the first week of the year
    -> DayOfWeek
    -- ^ the first day of each week
    -> Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekCalendarValid wt ws y wy dw = let
    d = fromWeekCalendar wt ws y wy dw
    in if toWeekCalendar wt ws d == (y,wy,dw) then Just d else Nothing

-- | Convert to ISO 8601 Week Date format. First element of result is year, second week number (1-53), third day of week (1 for Monday to 7 for Sunday).
-- Note that \"Week\" years are not quite the same as Gregorian years, as the first day of the year is always a Monday.
-- The first week of a year is the first week to contain at least four days in the corresponding Gregorian year.
toWeekDate :: Day -> (Year, WeekOfYear, Int)
toWeekDate d = let
    (y,wy,dw) = toWeekCalendar FirstMostWeek Monday d
    in (y,wy,fromEnum dw)

-- | Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
-- Invalid week and day values will be clipped to the correct range.
fromWeekDate :: Year -> WeekOfYear -> Int -> Day
fromWeekDate y wy dw = fromWeekCalendar FirstMostWeek Monday y wy (toEnum $ clip 1 7 dw)

-- | Bidirectional abstract constructor for ISO 8601 Week Date format.
-- Invalid week values will be clipped to the correct range.
pattern YearWeekDay :: Year -> WeekOfYear -> DayOfWeek -> Day
pattern YearWeekDay y wy dw <- (toWeekDate -> (y,wy,toEnum -> dw)) where
    YearWeekDay y wy dw = fromWeekDate y wy (fromEnum dw)

{-# COMPLETE YearWeekDay #-}

-- | Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
-- Invalid week and day values will return Nothing.
fromWeekDateValid :: Year -> WeekOfYear -> Int -> Maybe Day
fromWeekDateValid y wy dwr = do
    dw <- clipValid 1 7 dwr
    fromWeekCalendarValid FirstMostWeek Monday y wy (toEnum dw)

-- | Show in ISO 8601 Week Date format as yyyy-Www-d (e.g. \"2006-W46-3\").
showWeekDate :: Day -> String
showWeekDate date = (show4 y) ++ "-W" ++ (show2 w) ++ "-" ++ (show d)
  where
    (y, w, d) = toWeekDate date
