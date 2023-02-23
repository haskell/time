{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Calendar.Gregorian (
    -- * Year, month and day
    Year,
    pattern CommonEra,
    pattern BeforeCommonEra,
    MonthOfYear,
    pattern January,
    pattern February,
    pattern March,
    pattern April,
    pattern May,
    pattern June,
    pattern July,
    pattern August,
    pattern September,
    pattern October,
    pattern November,
    pattern December,
    DayOfMonth,

    -- * Gregorian calendar
    toGregorian,
    fromGregorian,
    pattern YearMonthDay,
    fromGregorianValid,
    showGregorian,
    gregorianMonthLength,
    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addGregorianMonthsClip,
    addGregorianMonthsRollOver,
    addGregorianYearsClip,
    addGregorianYearsRollOver,
    addGregorianDurationClip,
    addGregorianDurationRollOver,
    diffGregorianDurationClip,
    diffGregorianDurationRollOver,
    -- re-exported from OrdinalDate
    isLeapYear,
) where

import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Days
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Private
import Data.Time.Calendar.Types

-- | Convert to proleptic Gregorian calendar.
toGregorian :: Day -> (Year, MonthOfYear, DayOfMonth)
toGregorian date = (year, month, day)
  where
    (year, yd) = toOrdinalDate date
    (month, day) = dayOfYearToMonthAndDay (isLeapYear year) yd

-- | Convert from proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
fromGregorian :: Year -> MonthOfYear -> DayOfMonth -> Day
fromGregorian year month day = fromOrdinalDate year (monthAndDayToDayOfYear (isLeapYear year) month day)

-- | Bidirectional abstract constructor for the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <-
    (toGregorian -> (y, m, d))
    where
        YearMonthDay y m d = fromGregorian y m d

{-# COMPLETE YearMonthDay #-}

-- | Convert from proleptic Gregorian calendar.
-- Invalid values will return Nothing
fromGregorianValid :: Year -> MonthOfYear -> DayOfMonth -> Maybe Day
fromGregorianValid year month day = do
    doy <- monthAndDayToDayOfYearValid (isLeapYear year) month day
    fromOrdinalDateValid year doy

-- | Show in ISO 8601 format (yyyy-mm-dd)
showGregorian :: Day -> String
showGregorian date = (show4 y) ++ "-" ++ (show2 m) ++ "-" ++ (show2 d)
  where
    (y, m, d) = toGregorian date

-- | The number of days in a given month according to the proleptic Gregorian calendar.
gregorianMonthLength :: Year -> MonthOfYear -> DayOfMonth
gregorianMonthLength year = monthLength (isLeapYear year)

rolloverMonths :: (Year, Integer) -> (Year, MonthOfYear)
rolloverMonths (y, m) = (y + (div (m - 1) 12), fromIntegral (mod (m - 1) 12) + 1)

addGregorianMonths :: Integer -> Day -> (Year, MonthOfYear, DayOfMonth)
addGregorianMonths n day = (y', m', d)
  where
    (y, m, d) = toGregorian day
    (y', m') = rolloverMonths (y, fromIntegral m + n)

-- | Add months, with days past the last day of the month clipped to the last day.
-- For instance, 2005-01-30 + 1 month = 2005-02-28.
addGregorianMonthsClip :: Integer -> Day -> Day
addGregorianMonthsClip n day = fromGregorian y m d
  where
    (y, m, d) = addGregorianMonths n day

-- | Add months, with days past the last day of the month rolling over to the next month.
-- For instance, 2005-01-30 + 1 month = 2005-03-02.
addGregorianMonthsRollOver :: Integer -> Day -> Day
addGregorianMonthsRollOver n day = addDays (fromIntegral d - 1) (fromGregorian y m 1)
  where
    (y, m, d) = addGregorianMonths n day

-- | Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-02-28.
addGregorianYearsClip :: Integer -> Day -> Day
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)

-- | Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
-- For instance, 2004-02-29 + 2 years = 2006-03-01.
addGregorianYearsRollOver :: Integer -> Day -> Day
addGregorianYearsRollOver n = addGregorianMonthsRollOver (n * 12)

-- | Add months (clipped to last day), then add days
addGregorianDurationClip :: CalendarDiffDays -> Day -> Day
addGregorianDurationClip (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addGregorianDurationRollOver :: CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffGregorianDurationClip :: Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 = let
    (y1, m1, d1) = toGregorian day1
    (y2, m2, d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1
            then
                if d2 >= d1
                    then ymdiff
                    else ymdiff - 1
            else
                if d2 <= d1
                    then ymdiff
                    else ymdiff + 1
    dayAllowed = addGregorianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

-- | Calendrical difference, with as many whole months as possible.
diffGregorianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 = let
    (y1, m1, _) = toGregorian day1
    (y2, m2, _) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    findpos mdiff = let
        dayAllowed = addGregorianDurationRollOver (CalendarDiffDays mdiff 0) day1
        dd = diffDays day2 dayAllowed
        in if dd >= 0 then CalendarDiffDays mdiff dd else findpos (pred mdiff)
    findneg mdiff = let
        dayAllowed = addGregorianDurationRollOver (CalendarDiffDays mdiff 0) day1
        dd = diffDays day2 dayAllowed
        in if dd <= 0 then CalendarDiffDays mdiff dd else findpos (succ mdiff)
    in if day2 >= day1
        then findpos ymdiff
        else findneg ymdiff

-- orphan instance
instance Show Day where
    show = showGregorian

-- orphan instance
instance DayPeriod Year where
    periodFirstDay y = YearMonthDay y January 1
    periodLastDay y = YearMonthDay y December 31
    dayPeriod (YearMonthDay y _ _) = y
