{-# LANGUAGE Safe #-}

module Data.Time.Calendar (
    -- * Days
    Day (..),
    addDays,
    diffDays,

    -- * DayPeriod
    DayPeriod (..),
    periodAllDays,
    periodLength,
    periodFromDay,
    periodToDay,
    periodToDayClip,
    periodToDayValid,

    -- * Calendar Duration
    module Data.Time.Calendar.CalendarDiffDays,

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

    -- * Week
    DayOfWeek (..),
    dayOfWeek,
    dayOfWeekDiff,
    firstDayOfWeekOnAfter,
    weekAllDays,
    weekFirstDay,
    weekLastDay,
) where

import Data.Time.Calendar.CalendarDiffDays
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Types
import Data.Time.Calendar.Week
import Data.Time.Format ()
